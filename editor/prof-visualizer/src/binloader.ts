import { assert, assertEq } from "./utils";

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
    const markerAddrToNameBytes = new DataView(await blob.slice(Number(markerAddrToNameStart), -8).arrayBuffer());
    const markerAddrToName = new Map();
    const entryCount = readUsize(markerAddrToNameBytes, 0, targetPointerSize, isLittleEndian);
    let readptr = targetPointerSize;
    for (let i = 0; i < entryCount; i++) {
        const addr = readUsize(markerAddrToNameBytes, readptr, targetPointerSize, isLittleEndian);
        readptr += targetPointerSize;
        let namelen = 0;
        while (markerAddrToNameBytes.getUint8(readptr + namelen) !== 0) {
            namelen++;
        }
        const name = new TextDecoder().decode(markerAddrToNameBytes.buffer.slice(readptr, readptr + namelen));
        markerAddrToName.set(addr, name);
        readptr += namelen + 1;
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

const EMPTY_UINT8_ARRAY = new Uint8Array();

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

class TransformerState {
    #leftChunk: Uint8Array;
    #currentChunk: Uint8Array;
    #readptr: number;
    readonly #isLittleEndian: boolean;
    readonly #targetPointerSize: number;
    readonly #textReader = new TextDecoder("utf-8");

    constructor(isLittleEndian: boolean, targetPointerSize: number) {
        this.#leftChunk = EMPTY_UINT8_ARRAY;
        this.#currentChunk = EMPTY_UINT8_ARRAY;
        this.#readptr = 0;
        this.#isLittleEndian = isLittleEndian;
        this.#targetPointerSize = targetPointerSize;
    }

    clear() {
        this.#leftChunk = EMPTY_UINT8_ARRAY;
    }

    beginTransformChunk(chunk: Uint8Array) {
        this.#currentChunk = chunk;
        this.#readptr = 0;
    }

    canRead(byteLength: number): boolean {
        return this.#readptr + byteLength <= this.#currentChunk.length + this.#leftChunk.length;
    }

    setLeftChunk(): void {
        this.#leftChunk = this.#currentChunk.slice(this.#readptr);
    }

    tryNextMarkerTag(): MarkerTag | null {
        assertEq(this.#leftChunk.length, 0);

        if (!this.canRead(1)) {
            // cannot read
            return null;
        }

        const r = getMarkerTag(this.#currentChunk[this.#readptr]);
        this.#readptr += 1;
        return r;
    }

    tryNextAuxDataTypeTag(): AuxDataTypeTag | null {
        const n = this.tryNextU16();
        if (n === null) {
            // cannot read
            return null;
        }

        return getAuxDataTypeTag(n);
    }

    tryNextU16(): number | null {
        if (!this.canRead(2)) {
            // cannot read
            return null;
        }

        if (this.#leftChunk.length > 0) {
            // join
            assertEq(this.#readptr, 0);

            const buf = new Uint8Array(2);
            buf.set(this.#leftChunk, 0);
            buf.set(this.#currentChunk.slice(0, 2 - this.#leftChunk.length), this.#leftChunk.length);
            const dv = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
            this.#readptr = 2 - this.#leftChunk.length;
            this.#leftChunk = EMPTY_UINT8_ARRAY;

            return dv.getUint16(0, this.#isLittleEndian);
        } else {
            // straight read
            const dv = new DataView(this.#currentChunk.buffer, this.#currentChunk.byteOffset + this.#readptr, 2);
            this.#readptr += 2;

            return dv.getUint16(0, this.#isLittleEndian);
        }
    }

    tryNextU64(): bigint | null {
        if (!this.canRead(8)) {
            // cannot read
            return null;
        }

        if (this.#leftChunk.length > 0) {
            // join
            assertEq(this.#readptr, 0);

            const buf = new Uint8Array(8);
            buf.set(this.#leftChunk, 0);
            buf.set(this.#currentChunk.slice(0, 8 - this.#leftChunk.length), this.#leftChunk.length);
            const dv = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
            this.#readptr = 8 - this.#leftChunk.length;
            this.#leftChunk = EMPTY_UINT8_ARRAY;

            return dv.getBigUint64(0, this.#isLittleEndian);
        } else {
            // straight read
            const dv = new DataView(this.#currentChunk.buffer, this.#currentChunk.byteOffset + this.#readptr, 8);
            this.#readptr += 8;

            return dv.getBigUint64(0, this.#isLittleEndian);
        }
    }

    tryNextUsize(): bigint | null {
        if (!this.canRead(this.#targetPointerSize)) {
            // cannot read
            return null;
        }

        if (this.#leftChunk.length > 0) {
            // join
            assertEq(this.#readptr, 0);
            const leftChunkSize = this.#leftChunk.length;

            const buf = new Uint8Array(this.#targetPointerSize);
            buf.set(this.#leftChunk, 0);
            buf.set(this.#currentChunk.slice(0, this.#targetPointerSize - leftChunkSize), leftChunkSize);
            const dv = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
            this.#readptr = this.#targetPointerSize - leftChunkSize;
            this.#leftChunk = EMPTY_UINT8_ARRAY;

            return readUsize(dv, 0, this.#targetPointerSize, this.#isLittleEndian);
        } else {
            // straight read
            const dv = new DataView(
                this.#currentChunk.buffer,
                this.#currentChunk.byteOffset + this.#readptr,
                this.#targetPointerSize,
            );
            this.#readptr += this.#targetPointerSize;

            return readUsize(dv, 0, this.#targetPointerSize, this.#isLittleEndian);
        }
    }

    readText(): string | null {
        if (this.#leftChunk.length > 0) {
            // read from leftChunk(must not be consumed any bytes from currentChunk)
            assertEq(this.#readptr, 0);

            const zeroBytePoint = this.#leftChunk.indexOf(0);
            if (zeroBytePoint >= 0) {
                // terminal point found in this chunk
                const text = this.#textReader.decode(this.#leftChunk.slice(0, zeroBytePoint));
                this.#leftChunk = this.#leftChunk.slice(zeroBytePoint + 1);
                return text;
            }

            this.#textReader.decode(this.#leftChunk, { stream: true });
            this.#leftChunk = EMPTY_UINT8_ARRAY;
        }

        const zeroBytePoint = this.#currentChunk.slice(this.#readptr).indexOf(0);
        if (zeroBytePoint >= 0) {
            // terminal point found in this chunk
            const text = this.#textReader.decode(
                this.#currentChunk.slice(this.#readptr, zeroBytePoint + this.#readptr),
            );
            this.#readptr += zeroBytePoint + 1;
            return text;
        }

        // consume entire buf and return null(= reading not completed)
        this.#textReader.decode(this.#currentChunk.slice(this.#readptr), { stream: true });
        this.#readptr = this.#currentChunk.length;
        return null;
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

            // save leftChunk for boundary-crossing read
            state.setLeftChunk();
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
