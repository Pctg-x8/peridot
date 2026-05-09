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
        state: TransformerState
    ): MarkerTransformStep | null;

    static readonly MarkerTag = new (class extends MarkerTransformStep {
        override execute(
            controller: TransformStreamDefaultController<Marker>,
            state: TransformerState
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
                state: TransformerState
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
                state: TransformerState
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
                    state: TransformerState
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
                    state: TransformerState
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
                    private readonly markerAddr: bigint
                ) {
                    super();
                }

                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState
                ): MarkerTransformStep | null {
                    const sectionId = state.tryNextU64();
                    if (sectionId === null) {
                        // cannot read
                        return null;
                    }

                    controller.enqueue({
                        type: "Section.Begin",
                        timestamp: this.timestamp,
                        markerAddr: this.markerAddr,
                        sectionId,
                    });
                    return MarkerTransformStep.MarkerTag;
                }
            },
        },
        End: {
            Timestamp: new (class extends MarkerTransformStep {
                override execute(
                    controller: TransformStreamDefaultController<Marker>,
                    state: TransformerState
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
                    state: TransformerState
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
            state: TransformerState
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
        // should not read from leftChunk
        assertEq(this.#leftChunk.length, 0);

        if (!this.canRead(1)) {
            // cannot read
            return null;
        }

        const r = getMarkerTag(this.#currentChunk[this.#readptr]);
        this.#readptr += 1;
        return r;
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
                this.#targetPointerSize
            );
            this.#readptr += this.#targetPointerSize;

            return readUsize(dv, 0, this.#targetPointerSize, this.#isLittleEndian);
        }
    }
}

function newMarkerTransformStream(
    targetPointerSize: number,
    isLittleEndian: boolean
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

type MarkerTagByte = number;
const MARKER_TAG_BYTE_TERMINAL: MarkerTagByte = 0x00;
const MARKER_TAG_BYTE_EVENT: MarkerTagByte = 0x01;
const MARKER_TAG_BYTE_SECTION_BEGIN: MarkerTagByte = 0x02;
const MARKER_TAG_BYTE_SECTION_END: MarkerTagByte = 0x03;
type MarkerTag = "Terminal" | "Event" | "Section.Begin" | "Section.End";
export function getMarkerTag(byte: number): MarkerTag {
    switch (byte) {
        case MARKER_TAG_BYTE_TERMINAL:
            return "Terminal";
        case MARKER_TAG_BYTE_EVENT:
            return "Event";
        case MARKER_TAG_BYTE_SECTION_BEGIN:
            return "Section.Begin";
        case MARKER_TAG_BYTE_SECTION_END:
            return "Section.End";
        default:
            throw new InvalidBinError(`unknown marker tag: ${byte}`);
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
