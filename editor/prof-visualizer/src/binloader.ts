import { assertEq } from "./utils";

export class InvalidBinError extends Error {
    constructor(message: string) {
        super(`invalid bin: ${message}`);
    }
}

export type BinMetadata = {
    readonly targetPointerSize: number;
    readonly timestampFrequency: bigint;
    readonly markerAddrToName: Map<bigint, string>;
};

export async function loadBin(blob: Blob): Promise<[BinMetadata, ReadableStream<Marker>]> {
    const bom = new Uint8Array(await blob.slice(0, 2).arrayBuffer());
    let isLittleEndian: boolean;
    if (bom[0] == 0x01 && bom[1] == 0x02) {
        isLittleEndian = false;
    } else if (bom[0] == 0x02 && bom[1] == 0x01) {
        isLittleEndian = true;
    } else {
        throw new InvalidBinError("invalid heading bom");
    }

    const headerBytes = new DataView(await blob.slice(2, 11).arrayBuffer());
    const targetPointerSize = headerBytes.getUint8(0);
    const timestampFrequency = headerBytes.getBigInt64(1, isLittleEndian);
    console.log(targetPointerSize, timestampFrequency);

    const fixedFooterBytes = new DataView(await blob.slice(-8).arrayBuffer());
    const markerAddrToNameStart = fixedFooterBytes.getBigUint64(0, isLittleEndian);

    const markerAddrToNameBlob = blob.slice(Number(markerAddrToNameStart), -8);
    const entryCount = readUsize(
        new DataView(await markerAddrToNameBlob.slice(0, targetPointerSize).arrayBuffer()),
        0,
        targetPointerSize,
        isLittleEndian,
    );
    const markerAddrToName = new Map();
    for await (const [addr, name] of markerAddrToNameBlob
        .slice(targetPointerSize)
        .stream()
        .pipeThrough(
            MarkerAddrToNameTableTransformStep.createTransformStream(
                targetPointerSize,
                isLittleEndian,
                Number(entryCount),
            ),
        )) {
        markerAddrToName.set(addr, name);
    }

    console.log(markerAddrToName);

    const markerStream = blob
        .slice(11, Number(markerAddrToNameStart))
        .stream()
        .pipeThrough<Marker>(newMarkerTransformStream(targetPointerSize, isLittleEndian));

    return [
        {
            targetPointerSize,
            timestampFrequency,
            markerAddrToName,
        },
        markerStream,
    ];
}

abstract class MarkerAddrToNameTableTransformStep {
    static createTransformStream(
        targetPointerSize: number,
        isLittleEndian: boolean,
        count: number,
    ): TransformStream<Uint8Array, [bigint, string]> {
        const reader = new StreamingDataReader(isLittleEndian, targetPointerSize);
        let step: MarkerAddrToNameTableTransformStep = new MarkerAddrToNameTableTransformStep.Addr(count);

        return new TransformStream({
            start() {
                reader.clear();
                step = new MarkerAddrToNameTableTransformStep.Addr(count);
            },
            async transform(chunk, controller) {
                reader.pushChunk(chunk);
                while (true) {
                    const nextStep = step.execute(controller, reader);
                    if (nextStep === null) {
                        break;
                    }

                    step = nextStep;
                }
            },
            flush() {},
        });
    }

    /** returns null causing break loop(no state transition could not be made) */
    abstract execute(
        controller: TransformStreamDefaultController<[bigint, string]>,
        reader: StreamingDataReader,
    ): MarkerAddrToNameTableTransformStep | null;

    static readonly Addr = class extends MarkerAddrToNameTableTransformStep {
        constructor(private readonly leftCount: number) {
            super();
        }

        override execute(
            controller: TransformStreamDefaultController<[bigint, string]>,
            reader: StreamingDataReader,
        ): MarkerAddrToNameTableTransformStep | null {
            if (this.leftCount <= 0) {
                // read all
                controller.terminate();
                return null;
            }

            const addr = reader.tryNextUsize();
            return addr === null ? null : new MarkerAddrToNameTableTransformStep.Name(this.leftCount, addr);
        }
    };

    static readonly Name = class extends MarkerAddrToNameTableTransformStep {
        constructor(
            private readonly leftCount: number,
            private readonly addr: bigint,
        ) {
            super();
        }

        override execute(
            controller: TransformStreamDefaultController<[bigint, string]>,
            reader: StreamingDataReader,
        ): MarkerAddrToNameTableTransformStep | null {
            const name = reader.readText();
            if (name === null) {
                return null;
            }

            controller.enqueue([this.addr, name]);
            return new MarkerAddrToNameTableTransformStep.Addr(this.leftCount - 1);
        }
    };
}

abstract class MarkerTransformStep {
    /** returns null causing break loop(no state transition could not be made) */
    abstract execute(
        controller: TransformStreamDefaultController<Marker>,
        state: TransformerState,
    ): MarkerTransformStep | null;

    static readonly MarkerTag = new (class extends MarkerTransformStep {
        override execute(
            controller: TransformStreamDefaultController<Marker>,
            state: TransformerState,
        ): MarkerTransformStep | null {
            switch (state.tryNextMarkerTag()) {
                case null:
                    // cannot read
                    return null;
                case "Terminal":
                    return MarkerTransformStep.Terminated;
                case "Event":
                    return MarkerTransformStep.Event.Timestamp;
                case "Section.Begin":
                    return MarkerTransformStep.Section.Begin.Timestamp;
                case "Section.End":
                    return MarkerTransformStep.Section.End.Timestamp;
            }
        }
    })();

    static readonly Event = {
        Timestamp: new (class extends MarkerTransformStep {
            override execute(
                controller: TransformStreamDefaultController<Marker>,
                state: TransformerState,
            ): MarkerTransformStep | null {
                const timestamp = state.tryNextU64();
                if (timestamp === null) {
                    // cannot read
                    return null;
                }

                return new MarkerTransformStep.Event.MarkerAddr(timestamp);
            }
        })(),
        MarkerAddr: class extends MarkerTransformStep {
            constructor(private readonly timestamp: bigint) {
                super();
            }

            override execute(
                controller: TransformStreamDefaultController<Marker>,
                state: TransformerState,
            ): MarkerTransformStep | null {
                const markerAddr = state.tryNextUsize();
                if (markerAddr === null) {
                    // cannot read
                    return null;
                }

                controller.enqueue({
                    type: "Event",
                    timestamp: this.timestamp,
                    markerAddr,
                });
                return MarkerTransformStep.MarkerTag;
            }
        },
    } as const;

    static readonly Section = {
        Begin: {
            Timestamp: new (class extends MarkerTransformStep {
                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    const timestamp = state.tryNextU64();
                    if (timestamp === null) {
                        // cannot read
                        return null;
                    }

                    return new MarkerTransformStep.Section.Begin.MarkerAddr(timestamp);
                }
            })(),
            MarkerAddr: class extends MarkerTransformStep {
                constructor(private readonly timestamp: bigint) {
                    super();
                }

                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    const markerAddr = state.tryNextUsize();
                    if (markerAddr === null) {
                        // cannot read
                        return null;
                    }

                    return new MarkerTransformStep.Section.Begin.SectionID(this.timestamp, markerAddr);
                }
            },
            SectionID: class extends MarkerTransformStep {
                constructor(
                    private readonly timestamp: bigint,
                    private readonly markerAddr: bigint,
                ) {
                    super();
                }

                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    const sectionId = state.tryNextU64();
                    if (sectionId === null) {
                        // cannot read
                        return null;
                    }

                    return new MarkerTransformStep.Section.Begin.AuxDataTag(
                        this.timestamp,
                        this.markerAddr,
                        sectionId,
                        [],
                    );
                }
            },
            AuxDataTag: class extends MarkerTransformStep {
                constructor(
                    private readonly timestamp: bigint,
                    private readonly markerAddr: bigint,
                    private readonly sectionId: bigint,
                    private readonly collectedAuxData: unknown[],
                ) {
                    super();
                }

                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    switch (state.tryNextAuxDataTypeTag()) {
                        case null:
                            // cannot read
                            return null;
                        case "None":
                            // done here
                            controller.enqueue({
                                type: "Section.Begin",
                                timestamp: this.timestamp,
                                markerAddr: this.markerAddr,
                                sectionId: this.sectionId,
                                auxData: this.collectedAuxData,
                            });
                            return MarkerTransformStep.MarkerTag;
                        case "String":
                            return new MarkerTransformStep.Section.Begin.AuxDataString(
                                this.timestamp,
                                this.markerAddr,
                                this.sectionId,
                                this.collectedAuxData,
                            );
                    }
                }
            },
            AuxDataString: class extends MarkerTransformStep {
                constructor(
                    private readonly timestamp: bigint,
                    private readonly markerAddr: bigint,
                    private readonly sectionId: bigint,
                    private readonly collectedAuxData: unknown[],
                ) {
                    super();
                }

                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    const text = state.readText();
                    if (text === null) {
                        // still reading
                        return null;
                    }

                    return new MarkerTransformStep.Section.Begin.AuxDataTag(
                        this.timestamp,
                        this.markerAddr,
                        this.sectionId,
                        [...this.collectedAuxData, text],
                    );
                }
            },
        },
        End: {
            Timestamp: new (class extends MarkerTransformStep {
                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    const timestamp = state.tryNextU64();
                    if (timestamp === null) {
                        // cannot read
                        return null;
                    }

                    return new MarkerTransformStep.Section.End.SectionID(timestamp);
                }
            })(),
            SectionID: class extends MarkerTransformStep {
                constructor(private readonly timestamp: bigint) {
                    super();
                }

                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState,
                ): MarkerTransformStep | null {
                    const sectionId = state.tryNextU64();
                    if (sectionId === null) {
                        // cannot read
                        return null;
                    }

                    controller.enqueue({
                        type: "Section.End",
                        timestamp: this.timestamp,
                        sectionId,
                    });
                    return MarkerTransformStep.MarkerTag;
                }
            },
        },
    } as const;

    static readonly Terminated = new (class extends MarkerTransformStep {
        override execute(
            controller: TransformStreamDefaultController<Marker>,
            state: TransformerState,
        ): MarkerTransformStep | null {
            controller.terminate();
            return null;
        }
    })();
}

const EMPTY_UINT8_ARRAY = new Uint8Array();

function uint8ArrayToDataView<TArrayBuffer extends ArrayBufferLike>(
    buf: Uint8Array<TArrayBuffer>,
): DataView<TArrayBuffer> {
    return new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
}

class StreamingDataReader {
    readonly #chunk: { left: Uint8Array; current: Uint8Array } = {
        left: EMPTY_UINT8_ARRAY,
        current: EMPTY_UINT8_ARRAY,
    };
    #readptr = 0;
    readonly #isLittleEndian: boolean;
    readonly #nativeIntSize: number;
    readonly #textReader = new TextDecoder("utf-8");

    constructor(isLittleEndian: boolean, targetPointerSize: number) {
        this.#isLittleEndian = isLittleEndian;
        this.#nativeIntSize = targetPointerSize;
    }

    clear() {
        this.#chunk.left = EMPTY_UINT8_ARRAY;
    }

    pushChunk(chunk: Uint8Array) {
        // save left(not read) data
        this.#chunk.left = this.#chunk.current.slice(this.#readptr);

        this.#chunk.current = chunk;
        this.#readptr = 0;
    }

    get availableSize(): number {
        return this.#chunk.current.length + this.#chunk.left.length;
    }

    canRead(byteLength: number): boolean {
        return this.#readptr + byteLength <= this.availableSize;
    }

    #sliceContiguousBuffer(length: number): Uint8Array {
        if (this.#chunk.left.length > 0) {
            // needs join
            assertEq(this.#readptr, 0);

            const buf = new Uint8Array(length);
            buf.set(this.#chunk.left, 0);
            buf.set(this.#chunk.current.slice(0, length - this.#chunk.left.length), this.#chunk.left.length);
            this.#readptr = length - this.#chunk.left.length;
            this.#chunk.left = EMPTY_UINT8_ARRAY;
            return buf;
        }

        const buf = this.#chunk.current.slice(this.#readptr, this.#readptr + length);
        this.#readptr += length;
        return buf;
    }

    tryNextU8(): number | null {
        if (!this.canRead(1)) {
            // cannot read
            return null;
        }

        if (this.#chunk.left.length > 0) {
            // pop first byte from left chunk
            const v = this.#chunk.left[0];
            this.#chunk.left = this.#chunk.left.slice(1);
            return v;
        }

        const v = this.#chunk.current[this.#readptr];
        this.#readptr += 1;
        return v;
    }

    tryNextU16(): number | null {
        if (!this.canRead(2)) {
            // cannot read
            return null;
        }

        return uint8ArrayToDataView(this.#sliceContiguousBuffer(2)).getUint16(0, this.#isLittleEndian);
    }

    tryNextU64(): bigint | null {
        if (!this.canRead(8)) {
            // cannot read
            return null;
        }

        return uint8ArrayToDataView(this.#sliceContiguousBuffer(8)).getBigUint64(0, this.#isLittleEndian);
    }

    tryNextUsize(): bigint | null {
        if (!this.canRead(this.#nativeIntSize)) {
            // cannot read
            return null;
        }

        return readUsize(
            uint8ArrayToDataView(this.#sliceContiguousBuffer(this.#nativeIntSize)),
            0,
            this.#nativeIntSize,
            this.#isLittleEndian,
        );
    }

    readText(): string | null {
        if (this.#chunk.left.length > 0) {
            // read from leftChunk(must not be consumed any bytes from currentChunk)
            assertEq(this.#readptr, 0);

            const zeroBytePoint = this.#chunk.left.indexOf(0);
            if (zeroBytePoint >= 0) {
                // terminal point found in this chunk
                const text = this.#textReader.decode(this.#chunk.left.slice(0, zeroBytePoint));
                this.#chunk.left = this.#chunk.left.slice(zeroBytePoint + 1);
                return text;
            }

            this.#textReader.decode(this.#chunk.left, { stream: true });
            this.#chunk.left = EMPTY_UINT8_ARRAY;
        }

        const zeroBytePoint = this.#chunk.current.slice(this.#readptr).indexOf(0);
        if (zeroBytePoint >= 0) {
            // terminal point found in this chunk
            const text = this.#textReader.decode(
                this.#chunk.current.slice(this.#readptr, zeroBytePoint + this.#readptr),
            );
            this.#readptr += zeroBytePoint + 1;
            return text;
        }

        // consume entire buf and return null(= reading not completed)
        this.#textReader.decode(this.#chunk.current.slice(this.#readptr), { stream: true });
        this.#readptr = this.#chunk.current.length;
        return null;
    }
}

class TransformerState {
    readonly #reader: StreamingDataReader;

    constructor(isLittleEndian: boolean, targetPointerSize: number) {
        this.#reader = new StreamingDataReader(isLittleEndian, targetPointerSize);
    }

    clear() {
        this.#reader.clear();
    }

    beginTransformChunk(chunk: Uint8Array) {
        this.#reader.pushChunk(chunk);
    }

    tryNextMarkerTag(): MarkerTag | null {
        const v = this.#reader.tryNextU8();
        if (v === null) {
            // cannot read
            return null;
        }

        return getMarkerTag(v);
    }

    tryNextAuxDataTypeTag(): AuxDataTypeTag | null {
        const v = this.#reader.tryNextU16();
        if (v === null) {
            // cannot read
            return null;
        }

        return getAuxDataTypeTag(v);
    }

    tryNextU64(): bigint | null {
        return this.#reader.tryNextU64();
    }

    tryNextUsize(): bigint | null {
        return this.#reader.tryNextUsize();
    }

    readText(): string | null {
        return this.#reader.readText();
    }
}

function newMarkerTransformStream(
    targetPointerSize: number,
    isLittleEndian: boolean,
): TransformStream<Uint8Array, Marker> {
    const state = new TransformerState(isLittleEndian, targetPointerSize);
    let step: MarkerTransformStep = MarkerTransformStep.MarkerTag;

    return new TransformStream({
        start() {
            state.clear();
            step = MarkerTransformStep.MarkerTag;
        },
        async transform(chunk, controller) {
            state.beginTransformChunk(chunk);
            while (true) {
                const nextStep = step.execute(controller, state);
                if (nextStep === null) {
                    break;
                }

                step = nextStep;
            }
        },
        flush() {},
    });
}

type MarkerTagNum = number;
const MARKER_TAG_NUM_TERMINAL: MarkerTagNum = 0;
const MARKER_TAG_NUM_EVENT: MarkerTagNum = 1;
const MARKER_TAG_NUM_SECTION_BEGIN: MarkerTagNum = 2;
const MARKER_TAG_NUM_SECTION_END: MarkerTagNum = 3;
type MarkerTag = "Terminal" | "Event" | "Section.Begin" | "Section.End";
export function getMarkerTag(num: number): MarkerTag {
    switch (num) {
        case MARKER_TAG_NUM_TERMINAL:
            return "Terminal";
        case MARKER_TAG_NUM_EVENT:
            return "Event";
        case MARKER_TAG_NUM_SECTION_BEGIN:
            return "Section.Begin";
        case MARKER_TAG_NUM_SECTION_END:
            return "Section.End";
        default:
            throw new InvalidBinError(`unknown marker tag: ${num}`);
    }
}

type AuxDataTypeTagNum = number;
const AUX_DATA_TYPE_TAG_NUM_NONE: AuxDataTypeTagNum = 0;
const AUX_DATA_TYPE_TAG_NUM_STRING: AuxDataTypeTagNum = 1;
type AuxDataTypeTag = "None" | "String";
export function getAuxDataTypeTag(num: number): AuxDataTypeTag {
    switch (num) {
        case AUX_DATA_TYPE_TAG_NUM_NONE:
            return "None";
        case AUX_DATA_TYPE_TAG_NUM_STRING:
            return "String";
        default:
            throw new InvalidBinError(`unknown aux data type tag: ${num}`);
    }
}

export type Marker = EventMarker | SectionBeginMarker | SectionEndMarker;
export type EventMarker = {
    readonly type: "Event";
    readonly timestamp: bigint;
    readonly markerAddr: bigint;
};
export type SectionBeginMarker = {
    readonly type: "Section.Begin";
    readonly timestamp: bigint;
    readonly markerAddr: bigint;
    readonly sectionId: bigint;
    readonly auxData: unknown[];
};
export type SectionEndMarker = {
    readonly type: "Section.End";
    readonly timestamp: bigint;
    readonly sectionId: bigint;
};

function readUsize(dv: DataView, offset: number, targetPointerSize: number, isLittleEndian: boolean): bigint {
    switch (targetPointerSize) {
        case 4:
            return BigInt(dv.getUint32(offset, isLittleEndian));
        case 8:
            return dv.getBigUint64(offset, isLittleEndian);
        default:
            throw new Error(`unknown pointer size: ${targetPointerSize}`);
    }
}
