import {
    loadBin,
    type BinMetadata,
    type EventMarker,
    type MemoryStatsMarker,
    type SectionBeginMarker,
    type SectionEndMarker,
} from "./binloader";
import { bnMax, bnMin, bnRangeSlice, hasValue, type Range } from "./utils";
import type { TypedEventTarget } from "./utils/typedEvents";

export type ViewTab = "Main" | "PerRenderLoop";

export class Application extends (EventTarget as typeof TypedEventTarget<ApplicationEvents>) {
    static readonly instance = new Application();

    private constructor() {
        super();
    }

    #tab: ViewTab = "Main";
    get currentTab(): ViewTab {
        return this.#tab;
    }
    switchTab(tab: ViewTab) {
        if (this.#tab === tab) {
            // switch to same tab
            return;
        }

        this.#tab = tab;
        this.#typedDispatchEvent("tabSwitched", tab);
        this.#typedDispatchEvent("performAtomic");
    }

    #binMetadata: BinMetadata | null = null;
    #events: EventMarker[] = [];
    #sectionRanges: SectionRange[] = [];
    #memoryStats: MemoryStatsMarker[] = [];

    get currentBinMetadata(): BinMetadata | null {
        return this.#binMetadata;
    }

    get events(): EventMarker[] {
        return this.#events;
    }

    get sectionRanges(): SectionRange[] {
        return this.#sectionRanges;
    }

    get memoryStats(): MemoryStatsMarker[] {
        return this.#memoryStats;
    }

    computeChartTimestampRange(): Range<bigint> {
        return computeTimestampRange(this.#sectionRanges, this.#events, this.#memoryStats);
    }

    sliceSectionRange(timestampRange: Range<bigint>): SectionRange[] {
        return this.#sectionRanges
            .map(r => ({ ...r, timestamp: bnRangeSlice(r.timestamp, timestampRange) }))
            .filter(r => r.timestamp.end - r.timestamp.begin > 0n);
    }

    async load(blob: Blob): Promise<void> {
        const [binMetadata, markerStream] = await loadBin(blob);
        const sectionMarkers = [];
        const events = [];
        const memoryStats = [];
        for await (const m of markerStream) {
            if (m.type === "Section.Begin" || m.type === "Section.End") {
                sectionMarkers.push(m);
            }
            if (m.type == "Event") {
                events.push(m);
            }
            if (m.type == "MemoryStats") {
                memoryStats.push(m);
            }
        }
        console.log("finish", binMetadata, events, sectionMarkers, memoryStats);

        const sectionRanges = buildSectionRanges(sectionMarkers, binMetadata.markerAddrToName);
        console.log(sectionRanges);

        this.#binMetadata = binMetadata;
        this.#events = events;
        this.#sectionRanges = sectionRanges;
        this.#memoryStats = memoryStats;
        this.#typedDispatchEvent("sourceDataChanged", binMetadata);

        this.#typedDispatchEvent("performAtomic");
    }

    sync() {
        this.#typedDispatchEvent("performAtomic");
    }

    /** type safe dispatchEvent */
    #typedDispatchEvent<T extends keyof ApplicationEvents>(
        key: T,
        ...extraArgs: ApplicationEvents[T] extends void ? [] : [ApplicationEvents[T]]
    ): void {
        this.dispatchEvent(new CustomEvent(key, hasValue(extraArgs[0]) ? { detail: extraArgs[0] } : undefined));
    }
}

type ApplicationEvents = {
    performAtomic: void;
    tabSwitched: ViewTab;
    sourceDataChanged: BinMetadata;
};

export type SectionRange = {
    readonly markerName: string;
    readonly auxData: unknown[];
    readonly timestamp: Range<bigint>;
};

function buildSectionRanges(
    markers: (SectionBeginMarker | SectionEndMarker)[],
    markerAddrToName: Map<bigint, string>,
): SectionRange[] {
    const sectionById = new Map<bigint, { readonly begin?: SectionBeginMarker; readonly end?: SectionEndMarker }>();
    let maxTimestamp = 0n;
    for (const m of markers) {
        switch (m.type) {
            case "Section.Begin":
                {
                    const existingSection = sectionById.get(m.sectionId) ?? {};
                    if (hasValue(existingSection.begin)) {
                        console.error("section has begun twice", m, existingSection);
                        throw new Error(`section id ${m.sectionId} has begun twice`);
                    }

                    sectionById.set(m.sectionId, { ...existingSection, begin: m });
                }
                break;
            case "Section.End": {
                const existingSection = sectionById.get(m.sectionId) ?? {};
                if (hasValue(existingSection.end)) {
                    console.error(
                        "section ended twice",
                        m,
                        existingSection,
                        markerAddrToName.get(existingSection.begin!.markerAddr),
                    );
                    throw new Error(`section id ${m.sectionId} ended twice`);
                }

                sectionById.set(m.sectionId, { ...existingSection, end: m });
            }
        }

        if (m.timestamp > maxTimestamp) {
            maxTimestamp = m.timestamp;
        }
    }

    return sectionById
        .values()
        .map(v => {
            let markerName: string;
            if (hasValue(v.begin)) {
                markerName = markerAddrToName.get(v.begin.markerAddr) ?? "<Unknown Section>";
            } else {
                markerName = "<Unknown Section>";
            }

            return {
                markerName,
                auxData: v.begin?.auxData ?? [],
                timestamp: {
                    begin: v.begin?.timestamp ?? 0n,
                    end: v.end?.timestamp ?? maxTimestamp,
                },
            };
        })
        .toArray();
}

function computeTimestampRange(
    sectionRanges: SectionRange[],
    events: EventMarker[],
    memoryStats: MemoryStatsMarker[],
): Range<bigint> {
    let minTimestamp: bigint | null = null;
    let maxTimestamp: bigint | null = null;

    for (const r of sectionRanges) {
        minTimestamp = minTimestamp === null ? r.timestamp.begin : bnMin(minTimestamp, r.timestamp.begin);
        maxTimestamp = maxTimestamp === null ? r.timestamp.end : bnMax(maxTimestamp, r.timestamp.end);
    }

    for (const e of events) {
        minTimestamp = minTimestamp === null ? e.timestamp : bnMin(minTimestamp, e.timestamp);
        maxTimestamp = maxTimestamp === null ? e.timestamp : bnMax(maxTimestamp, e.timestamp);
    }

    for (const m of memoryStats) {
        minTimestamp = minTimestamp === null ? m.timestamp : bnMin(minTimestamp, m.timestamp);
        maxTimestamp = maxTimestamp === null ? m.timestamp : bnMax(maxTimestamp, m.timestamp);
    }

    return { begin: minTimestamp ?? 0n, end: maxTimestamp ?? 0n };
}
