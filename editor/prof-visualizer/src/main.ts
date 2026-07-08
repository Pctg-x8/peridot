import {
    loadBin,
    type EventMarker,
    type MemoryStatsMarker,
    type SectionBeginMarker,
    type SectionEndMarker,
} from "./binloader";
import { bnMax, bnMin, hasValue } from "./utils";
import { PrefixedViewGroup, ViewElement } from "./viewHelper";

document.addEventListener("DOMContentLoaded", () => {
    new HeaderPresenter().launch();
});

class HeaderPresenter {
    static readonly #ViewGroup = new PrefixedViewGroup("Header");

    readonly #inputFileView = HeaderPresenter.#ViewGroup.view<HTMLInputElement>("InputFile");
    readonly #loadButtonView = HeaderPresenter.#ViewGroup.view<HTMLButtonElement>("LoadButton");
    readonly #chartSurface = new ViewElement<SVGGElement>("ChartSurface");
    readonly #chartContainer = new ViewElement<SVGGElement>("ChartContainer");

    launch() {
        this.#loadButtonView.ref.addEventListener("click", async () => {
            const file = this.#inputFileView.ref.files?.item(0);
            if (!hasValue(file)) {
                alert("ファイルが選択されていません");
                return;
            }

            const [binMetadata, markerStream] = await loadBin(file);
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
            const chartTimestampRange = computeTimestampRange(sectionRanges, events, memoryStats);
            const timelineChartModel = buildTimelineChartModel(
                sectionRanges,
                events,
                chartTimestampRange,
                binMetadata.timestampFrequency,
                binMetadata.markerAddrToName,
            );
            console.log(timelineChartModel);
            const memoryChartModel = buildMemoryChartModel(
                memoryStats,
                chartTimestampRange,
                binMetadata.timestampFrequency,
            );
            console.log(memoryChartModel);

            // 最大値よりちょっと大きめに取る
            const lineChartContentHeight = Math.max(
                memoryChartModel.totalResident
                    .map(x => x.y + 500.0 * MEMORY_CHART_HEIGHT_PER_BYTES)
                    .reduce((a, b) => Math.max(a, b), TIMELINE_CHART_TOP_MARGIN),
                memoryChartModel.totalReserved
                    .map(x => x.y + 500.0 * MEMORY_CHART_HEIGHT_PER_BYTES)
                    .reduce((a, b) => Math.max(a, b), TIMELINE_CHART_TOP_MARGIN),
            );
            const timelineChartContentHeight = timelineChartModel.barRects
                .map(x => x.top + x.height)
                .reduce((a, b) => Math.max(a, b), 0);

            const d = new DocumentFragment();

            const memoryTotalResidentLines = document.createElementNS("http://www.w3.org/2000/svg", "polyline");
            memoryTotalResidentLines.setAttribute(
                "points",
                memoryChartModel.totalResident.map(x => `${x.x},${lineChartContentHeight - x.y}`).join(" "),
            );
            memoryTotalResidentLines.setAttribute("stroke-width", "1");
            memoryTotalResidentLines.setAttribute("stroke", "#ccc");
            memoryTotalResidentLines.setAttribute("fill", "transparent");
            d.appendChild(memoryTotalResidentLines);
            for (const p of memoryChartModel.totalResident) {
                const g = document.createElementNS("http://www.w3.org/2000/svg", "g");

                const point = document.createElementNS("http://www.w3.org/2000/svg", "circle");
                point.setAttribute("cx", p.x.toString());
                point.setAttribute("cy", (lineChartContentHeight - p.y).toString());
                point.setAttribute("r", "2");
                point.setAttribute("fill", "#ccc");
                g.appendChild(point);

                const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
                title.textContent = p.tooltipText;
                g.appendChild(title);

                d.appendChild(g);
            }

            const memoryTotalReservedLines = document.createElementNS("http://www.w3.org/2000/svg", "polyline");
            memoryTotalReservedLines.setAttribute(
                "points",
                memoryChartModel.totalReserved.map(x => `${x.x},${lineChartContentHeight - x.y}`).join(" "),
            );
            memoryTotalReservedLines.setAttribute("stroke-width", "1");
            memoryTotalReservedLines.setAttribute("stroke", "#ccc");
            memoryTotalReservedLines.setAttribute("fill", "transparent");
            d.appendChild(memoryTotalReservedLines);
            for (const p of memoryChartModel.totalReserved) {
                const g = document.createElementNS("http://www.w3.org/2000/svg", "g");

                const point = document.createElementNS("http://www.w3.org/2000/svg", "circle");
                point.setAttribute("cx", p.x.toString());
                point.setAttribute("cy", (lineChartContentHeight - p.y).toString());
                point.setAttribute("r", "2");
                point.setAttribute("fill", "#ccc");
                g.appendChild(point);

                const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
                title.textContent = p.tooltipText;
                g.appendChild(title);

                d.appendChild(g);
            }

            const memoryTotalPrivateResidentLines = document.createElementNS("http://www.w3.org/2000/svg", "polyline");
            memoryTotalPrivateResidentLines.setAttribute(
                "points",
                memoryChartModel.totalPrivateResident.map(x => `${x.x},${lineChartContentHeight - x.y}`).join(" "),
            );
            memoryTotalPrivateResidentLines.setAttribute("stroke-width", "1");
            memoryTotalPrivateResidentLines.setAttribute("stroke", "#ccc");
            memoryTotalPrivateResidentLines.setAttribute("fill", "transparent");
            d.appendChild(memoryTotalPrivateResidentLines);
            for (const p of memoryChartModel.totalPrivateResident) {
                const g = document.createElementNS("http://www.w3.org/2000/svg", "g");

                const point = document.createElementNS("http://www.w3.org/2000/svg", "circle");
                point.setAttribute("cx", p.x.toString());
                point.setAttribute("cy", (lineChartContentHeight - p.y).toString());
                point.setAttribute("r", "2");
                point.setAttribute("fill", "#ccc");
                g.appendChild(point);

                const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
                title.textContent = p.tooltipText;
                g.appendChild(title);

                d.appendChild(g);
            }

            const timelineTopLine = document.createElementNS("http://www.w3.org/2000/svg", "line");
            timelineTopLine.setAttribute("x1", "0");
            timelineTopLine.setAttribute("y1", lineChartContentHeight.toString());
            timelineTopLine.setAttribute("x2", "100%");
            timelineTopLine.setAttribute("y2", lineChartContentHeight.toString());
            timelineTopLine.setAttribute("stroke", "#666");
            timelineTopLine.setAttribute("stroke-width", "1");
            timelineTopLine.setAttribute("fill", "transparent");
            d.appendChild(timelineTopLine);
            let barRectId = 0;
            for (const r of timelineChartModel.barRects) {
                const hue =
                    r.labelText
                        .split("")
                        .map(c => c.charCodeAt(0) * 7)
                        .reduce((a, b) => a + b, 0) % 360;
                const top = lineChartContentHeight + r.top;

                const clip = document.createElementNS("http://www.w3.org/2000/svg", "clipPath");
                const clipId = (clip.id = `barRectClip-${barRectId}`);
                const rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
                rect.setAttribute("x", r.left.toString());
                rect.setAttribute("y", top.toString());
                rect.setAttribute("width", r.width.toString());
                rect.setAttribute("height", r.height.toString());
                clip.appendChild(rect);
                d.appendChild(clip);

                const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
                const e = document.createElementNS("http://www.w3.org/2000/svg", "rect");
                e.setAttribute("x", r.left.toString());
                e.setAttribute("y", top.toString());
                e.setAttribute("width", r.width.toString());
                e.setAttribute("height", r.height.toString());
                e.style.fill = `oklch(100% 0.25 ${hue})`;
                e.setAttribute("stroke", "transparent");
                e.setAttribute("stroke-width", "0");
                const tx = document.createElementNS("http://www.w3.org/2000/svg", "text");
                tx.textContent = r.labelText;
                tx.setAttribute("x", r.left.toString());
                tx.setAttribute("y", (top + r.height * 0.5).toString());
                tx.setAttribute("clip-path", `url(#${clipId})`);
                tx.setAttribute("dominant-baseline", "middle");
                const t = document.createElementNS("http://www.w3.org/2000/svg", "title");
                t.textContent = r.tooltipText;

                g.appendChild(e);
                g.appendChild(tx);
                g.appendChild(t);
                d.appendChild(g);
                barRectId += 1;
            }

            for (const l of timelineChartModel.eventLines) {
                const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
                text.textContent = l.labelText;
                text.setAttribute("x", l.left.toString());
                text.setAttribute("y", "0");
                text.setAttribute("dominant-baseline", "text-top");
                text.classList.add("eventLine");

                const line = document.createElementNS("http://www.w3.org/2000/svg", "line");
                line.setAttribute("x1", l.left.toString());
                line.setAttribute("x2", l.left.toString());
                line.setAttribute("y1", "0");
                line.setAttribute("y2", "100%");
                line.setAttribute("stroke-width", "1");
                line.setAttribute("stroke", "#999");

                d.appendChild(text);
                d.appendChild(line);
            }

            const w =
                timestampToSecs(chartTimestampRange.end - chartTimestampRange.start, binMetadata.timestampFrequency) *
                TIMELINE_CHART_WIDTH_PER_SEC;
            const h = lineChartContentHeight + timelineChartContentHeight;
            this.#chartSurface.ref.setAttribute("width", (w * window.devicePixelRatio).toString());
            this.#chartSurface.ref.setAttribute("height", (h * window.devicePixelRatio).toString());
            this.#chartSurface.ref.setAttribute("viewBox", `0 0 ${w} ${h}`);

            this.#chartContainer.ref.replaceChildren(d);
        });
    }
}

type Range<T> = {
    readonly begin: T;
    readonly end: T;
};

type SectionRange = {
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

const TIMELINE_CHART_BAR_THICKNESS: number = 12.0;
const TIMELINE_CHART_WIDTH_PER_SEC: number = 128.0 * 50.0;
const TIMELINE_CHART_TOP_MARGIN: number = 120.0;

export function timestampToSecs(timestamp: bigint, freq: bigint): number {
    return Number(timestamp) / Number(freq);
}

function computeTimestampRange(
    sectionRanges: SectionRange[],
    events: EventMarker[],
    memoryStats: MemoryStatsMarker[],
): { readonly start: bigint; readonly end: bigint } {
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

    return { start: minTimestamp ?? 0n, end: maxTimestamp ?? 0n };
}

type BarRect = {
    readonly labelText: string;
    readonly tooltipText: string;
    readonly left: number;
    readonly top: number;
    readonly width: number;
    readonly height: number;
};
type EventLine = {
    readonly labelText: string;
    readonly left: number;
};
type TimelineChartModel = {
    readonly barRects: BarRect[];
    readonly eventLines: EventLine[];
};
function buildTimelineChartModel(
    sectionRanges: SectionRange[],
    events: EventMarker[],
    timestampRange: { readonly start: bigint; readonly end: bigint },
    timestampFrequency: bigint,
    markerAddrToName: Map<bigint, string>,
): TimelineChartModel {
    const sortedRanges = sectionRanges.toSorted((a, b) => Number(a.timestamp.begin - b.timestamp.begin));
    const rects: BarRect[] = [];
    const endTimestampStack: bigint[] = [];
    for (const r of sortedRanges) {
        while (endTimestampStack.length > 0) {
            if (r.timestamp.begin < endTimestampStack.at(-1)!) {
                // stack onto this
                break;
            }

            endTimestampStack.pop();
        }

        if (r.timestamp.end < timestampRange.start || timestampRange.end < r.timestamp.begin) {
            // completely out of range
            continue;
        }

        const durationNs = ((r.timestamp.end - r.timestamp.begin) * 1_000_000_000n) / timestampFrequency;
        const labelText = formatSectionText(r);
        const tooltipText = `${labelText} (${displayNanos(durationNs)})`;
        const left =
            timestampToSecs(r.timestamp.begin - timestampRange.start, timestampFrequency) *
            TIMELINE_CHART_WIDTH_PER_SEC;
        const right =
            timestampToSecs(r.timestamp.end - timestampRange.start, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;
        rects.push({
            labelText,
            tooltipText,
            left,
            top: endTimestampStack.length * TIMELINE_CHART_BAR_THICKNESS,
            width: right - left,
            height: TIMELINE_CHART_BAR_THICKNESS,
        });
        endTimestampStack.push(r.timestamp.end);
    }

    const eventLines = events
        .toSorted((a, b) => Number(a.timestamp - b.timestamp))
        .map(e => {
            if (e.timestamp < timestampRange.start || timestampRange.end < e.timestamp) {
                // out of range
                return null;
            }

            const labelText = markerAddrToName.get(e.markerAddr)!;
            const left =
                timestampToSecs(e.timestamp - timestampRange.start, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;

            return { labelText, left };
        })
        .filter(x => x !== null);

    return { barRects: rects, eventLines };
}

const MEMORY_CHART_HEIGHT_PER_BYTES: number = 1.0 / 1_000_000.0;

type ChartPoint = {
    readonly x: number;
    readonly y: number;
    readonly tooltipText: string;
};
type MemoryChartModel = {
    readonly totalResident: ChartPoint[];
    readonly totalReserved: ChartPoint[];
    readonly totalPrivateResident: ChartPoint[];
};
export function buildMemoryChartModel(
    memoryStats: MemoryStatsMarker[],
    timestampRange: { readonly start: bigint; readonly end: bigint },
    timestampFrequency: bigint,
): MemoryChartModel {
    const totalResident = [];
    const totalReserved = [];
    const totalPrivateResident = [];

    let lastBeyond = false;
    for (const stat of memoryStats.toSorted((a, b) => Number(a.timestamp - b.timestamp))) {
        const past = stat.timestamp < timestampRange.start;
        const beyond = stat.timestamp >= timestampRange.end;

        const x =
            timestampToSecs(stat.timestamp - timestampRange.start, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;

        if (past) {
            // これより前の点は不要
            totalResident.splice(0);
            totalReserved.splice(0);
        }

        totalResident.push({
            x,
            y: Number(stat.totalResidentBytes) * MEMORY_CHART_HEIGHT_PER_BYTES,
            tooltipText: `Memory: Total Resident Bytes: ${displayByteSize(stat.totalResidentBytes)}`,
        });
        totalReserved.push({
            x,
            y: Number(stat.totalReservedBytes) * MEMORY_CHART_HEIGHT_PER_BYTES,
            tooltipText: `Memory: Total Reserved Bytes: ${displayByteSize(stat.totalReservedBytes)}`,
        });
        totalPrivateResident.push({
            x,
            y: Number(stat.totalPrivateResidentBytes) * MEMORY_CHART_HEIGHT_PER_BYTES,
            tooltipText: `Memory: Total Private Resident Bytes: ${displayByteSize(stat.totalPrivateResidentBytes)}`,
        });

        if (lastBeyond && beyond) {
            // 2点連続で右端を超えた
            break;
        }
        lastBeyond = beyond;
    }

    return { totalResident, totalReserved, totalPrivateResident };
}

function formatSectionText(section: SectionRange): string {
    let text = section.markerName;
    if (section.auxData.length > 0) {
        text += `: ${section.auxData.join(", ")}`;
    }

    return text;
}

function displayNanos(ns: bigint): string {
    let unit = "ns";
    let val = Number(ns);
    if (val >= 1000) {
        unit = "us";
        val /= 1000;
    }
    if (val >= 1000) {
        unit = "ms";
        val /= 1000;
    }

    if (unit === "ns") {
        // no conversion occured
        return `${ns} ns`;
    }

    return `${val.toFixed(2)} ${unit}`;
}

function displayByteSize(bytes: bigint): string {
    let unit = "B";
    let val = Number(bytes);
    if (val >= 1000) {
        unit = "KB";
        val /= 1024;
    }
    if (val >= 1000) {
        unit = "MB";
        val /= 1024;
    }
    if (val >= 1000) {
        unit = "GB";
        val /= 1024;
    }

    if (unit === "B") {
        // no conversion occured
        return `${bytes} B`;
    }

    return `${val.toFixed(3)} ${unit}`;
}
