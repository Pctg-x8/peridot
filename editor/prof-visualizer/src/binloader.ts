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

type MarkerTransformStep =
    | { readonly step: "MarkerTag" }
    | { readonly step: "Event.Timestamp" }
    | { readonly step: "Event.MarkerAddr"; readonly timestamp: bigint }
    | { readonly step: "Section.Begin.Timestamp" }
    | { readonly step: "Section.Begin.MarkerAddr"; readonly timestamp: bigint }
    | { readonly step: "Section.Begin.SectionID"; readonly timestamp: bigint; readonly markerAddr: bigint }
    | { readonly step: "Section.End.Timestamp" }
    | { readonly step: "Section.End.SectionID"; readonly timestamp: bigint }
    | { readonly step: "Terminated" };
type TransformerState = {
    leftChunk: Uint8Array;
    step: MarkerTransformStep;
    readptr: number;
};
const EMPTY_UINT8_ARRAY = new Uint8Array();

function newMarkerTransformStream(
    targetPointerSize: number,
    isLittleEndian: boolean
): TransformStream<Uint8Array, Marker> {
    const state: TransformerState = {
        leftChunk: EMPTY_UINT8_ARRAY,
        step: { step: "MarkerTag" },
        readptr: 0,
    };

    return new TransformStream({
        start() {
            state.leftChunk = EMPTY_UINT8_ARRAY;
            state.step = { step: "MarkerTag" };
        },
        async transform(chunk, controller) {
            state.readptr = 0;
            while (true) {
                switch (state.step.step) {
                    case "MarkerTag":
                        if (state.readptr + 1 > chunk.length) {
                            // cannot read
                            return;
                        } else {
                            switch (getMarkerTag(chunk[state.readptr])) {
                                case "Terminal":
                                    state.step = { step: "Terminated" };
                                    break;
                                case "Event":
                                    state.step = { step: "Event.Timestamp" };
                                    break;
                                case "Section.Begin":
                                    state.step = { step: "Section.Begin.Timestamp" };
                                    break;
                                case "Section.End":
                                    state.step = { step: "Section.End.Timestamp" };
                                    break;
                            }

                            state.readptr += 1;
                        }
                        break;
                    case "Event.Timestamp":
                        if (state.readptr + 8 > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let timestamp: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(8);
                                buf.set(state.leftChunk, 0);
                                buf.set(chunk.slice(0, 8 - state.leftChunk.length), state.leftChunk.length);
                                timestamp = new DataView(buf.buffer, buf.byteOffset, buf.byteLength).getBigInt64(
                                    0,
                                    isLittleEndian
                                );
                                state.readptr = 8 - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                timestamp = new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength).getBigInt64(
                                    state.readptr,
                                    isLittleEndian
                                );
                                state.readptr += 8;
                            }

                            state.step = { step: "Event.MarkerAddr", timestamp };
                        }
                        break;
                    case "Event.MarkerAddr":
                        if (state.readptr + targetPointerSize > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let markerAddr: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(targetPointerSize);
                                buf.set(state.leftChunk, 0);
                                buf.set(
                                    chunk.slice(0, targetPointerSize - state.leftChunk.length),
                                    state.leftChunk.length
                                );
                                markerAddr = readUsize(
                                    new DataView(buf.buffer, buf.byteOffset, buf.byteLength),
                                    0,
                                    targetPointerSize,
                                    isLittleEndian
                                );
                                state.readptr = targetPointerSize - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                markerAddr = readUsize(
                                    new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength),
                                    state.readptr,
                                    targetPointerSize,
                                    isLittleEndian
                                );
                                state.readptr += targetPointerSize;
                            }
                            controller.enqueue({
                                type: "Event",
                                timestamp: state.step.timestamp,
                                markerAddr,
                            });
                            state.step = { step: "MarkerTag" };
                        }
                        break;
                    case "Section.Begin.Timestamp":
                        if (state.readptr + 8 > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let timestamp: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(8);
                                buf.set(state.leftChunk, 0);
                                buf.set(chunk.slice(0, 8 - state.leftChunk.length), state.leftChunk.length);
                                timestamp = new DataView(buf.buffer, buf.byteOffset, buf.byteLength).getBigInt64(
                                    0,
                                    isLittleEndian
                                );
                                state.readptr = 8 - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                timestamp = new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength).getBigInt64(
                                    state.readptr,
                                    isLittleEndian
                                );
                                state.readptr += 8;
                            }

                            state.step = { step: "Section.Begin.MarkerAddr", timestamp };
                        }
                        break;
                    case "Section.Begin.MarkerAddr":
                        if (state.readptr + targetPointerSize > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let markerAddr: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(targetPointerSize);
                                buf.set(state.leftChunk, 0);
                                buf.set(
                                    chunk.slice(0, targetPointerSize - state.leftChunk.length),
                                    state.leftChunk.length
                                );
                                markerAddr = readUsize(
                                    new DataView(buf.buffer, buf.byteOffset, buf.byteLength),
                                    0,
                                    targetPointerSize,
                                    isLittleEndian
                                );
                                state.readptr = targetPointerSize - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                markerAddr = readUsize(
                                    new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength),
                                    state.readptr,
                                    targetPointerSize,
                                    isLittleEndian
                                );
                                state.readptr += targetPointerSize;
                            }
                            state.step = {
                                step: "Section.Begin.SectionID",
                                timestamp: state.step.timestamp,
                                markerAddr,
                            };
                        }
                        break;
                    case "Section.Begin.SectionID":
                        if (state.readptr + 8 > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let sectionId: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(8);
                                buf.set(state.leftChunk, 0);
                                buf.set(chunk.slice(0, 8 - state.leftChunk.length), state.leftChunk.length);
                                sectionId = new DataView(buf.buffer, buf.byteOffset, buf.byteLength).getBigUint64(
                                    0,
                                    isLittleEndian
                                );
                                state.readptr = 8 - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                sectionId = new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength).getBigUint64(
                                    state.readptr,
                                    isLittleEndian
                                );
                                state.readptr += 8;
                            }

                            controller.enqueue({
                                type: "Section.Begin",
                                timestamp: state.step.timestamp,
                                markerAddr: state.step.markerAddr,
                                sectionId,
                            });
                            state.step = { step: "MarkerTag" };
                        }
                        break;
                    case "Section.End.Timestamp":
                        if (state.readptr + 8 > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let timestamp: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(8);
                                buf.set(state.leftChunk, 0);
                                buf.set(chunk.slice(0, 8 - state.leftChunk.length), state.leftChunk.length);
                                timestamp = new DataView(buf.buffer, buf.byteOffset, buf.byteLength).getBigInt64(
                                    0,
                                    isLittleEndian
                                );
                                state.readptr = 8 - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                timestamp = new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength).getBigInt64(
                                    state.readptr,
                                    isLittleEndian
                                );
                                state.readptr += 8;
                            }

                            state.step = { step: "Section.End.SectionID", timestamp };
                        }
                        break;
                    case "Section.End.SectionID":
                        if (state.readptr + 8 > chunk.length) {
                            // cannot read
                            state.leftChunk = chunk.slice(state.readptr);
                            return;
                        } else {
                            let sectionId: bigint;
                            if (state.leftChunk.length > 0) {
                                // join
                                const buf = new Uint8Array(8);
                                buf.set(state.leftChunk, 0);
                                buf.set(chunk.slice(0, 8 - state.leftChunk.length), state.leftChunk.length);
                                sectionId = new DataView(buf.buffer, buf.byteOffset, buf.byteLength).getBigUint64(
                                    0,
                                    isLittleEndian
                                );
                                state.readptr = 8 - state.leftChunk.length;
                                state.leftChunk = new Uint8Array();
                            } else {
                                // straight read
                                sectionId = new DataView(chunk.buffer, chunk.byteOffset, chunk.byteLength).getBigUint64(
                                    state.readptr,
                                    isLittleEndian
                                );
                                state.readptr += 8;
                            }

                            controller.enqueue({
                                type: "Section.End",
                                timestamp: state.step.timestamp,
                                sectionId,
                            });
                            state.step = { step: "MarkerTag" };
                        }
                        break;
                    case "Terminated":
                        controller.terminate();
                        return;
                }
            }
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
