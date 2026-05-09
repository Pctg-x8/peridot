import { loadBin, type EventMarker, type SectionBeginMarker, type SectionEndMarker } from "./binloader";
import { bnMin, hasValue } from "./utils";
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
            for await (const m of markerStream) {
                console.log(m);
                if (m.type === "Section.Begin" || m.type === "Section.End") {
                    sectionMarkers.push(m);
                }
                if (m.type == "Event") {
                    events.push(m);
                }
            }
            console.log("finish", binMetadata);

            const sectionRanges = buildSectionRanges(sectionMarkers, binMetadata.markerAddrToName);
            console.log(sectionRanges);
            const timelineChartModel = buildTimelineChartModel(
                sectionRanges,
                events,
                binMetadata.timestampFrequency,
                binMetadata.markerAddrToName,
            );
            console.log(timelineChartModel);

            const chartContentHeight = Math.max(
                ...timelineChartModel.barRects.map(x => TIMELINE_CHART_TOP_MARGIN + x.top + x.height),
            );

            const d = new DocumentFragment();

            const timelineTopLine = document.createElementNS("http://www.w3.org/2000/svg", "line");
            timelineTopLine.setAttribute("x1", "0");
            timelineTopLine.setAttribute("y1", TIMELINE_CHART_TOP_MARGIN.toString());
            timelineTopLine.setAttribute("x2", "100%");
            timelineTopLine.setAttribute("y2", TIMELINE_CHART_TOP_MARGIN.toString());
            timelineTopLine.setAttribute("stroke", "#666");
            timelineTopLine.setAttribute("stroke-width", "1");
            d.appendChild(timelineTopLine);
            let barRectId = 0;
            for (const r of timelineChartModel.barRects) {
                const hue =
                    r.labelText
                        .split("")
                        .map(c => c.charCodeAt(0) * 7)
                        .reduce((a, b) => a + b, 0) % 360;
                const top = TIMELINE_CHART_TOP_MARGIN + r.top;

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

            const w = timelineChartModel.barRects.reduce((a, b) => Math.max(a, b.left + b.width), 0);
            const h = chartContentHeight;
            this.#chartSurface.ref.setAttribute("width", w.toString());
            this.#chartSurface.ref.setAttribute("height", h.toString());
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
                        throw new Error(`section id ${m.sectionId} has begun twice`);
                    }

                    sectionById.set(m.sectionId, { ...existingSection, begin: m });
                }
                break;
            case "Section.End": {
                const existingSection = sectionById.get(m.sectionId) ?? {};
                if (hasValue(existingSection.end)) {
                    throw new Error(`section id ${m.sectionId} has begun twice`);
                }

                sectionById.set(m.sectionId, { ...existingSection, end: m });
            }
        }

        if (m.timestamp > maxTimestamp) {
            maxTimestamp = m.timestamp;
        }
    }

    return sectionById
        .entries()
        .map(([k, v]) => {
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
const TIMELINE_CHART_WIDTH_PER_SEC: number = 128.0 * 100.0;
const TIMELINE_CHART_TOP_MARGIN: number = 120.0;

export function timestampToSecs(timestamp: bigint, freq: bigint): number {
    return Number(timestamp) / Number(freq);
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
    timestampFrequency: bigint,
    markerAddrToName: Map<bigint, string>,
): TimelineChartModel {
    const sortedRanges = sectionRanges.toSorted((a, b) => Number(a.timestamp.begin - b.timestamp.begin));
    const rects: BarRect[] = [];
    const endTimestampStack: bigint[] = [];
    const baseTimestamp = events.reduce((a, b) => bnMin(a, b.timestamp), sortedRanges[0].timestamp.begin);
    for (const r of sortedRanges) {
        while (endTimestampStack.length > 0) {
            if (r.timestamp.begin < endTimestampStack.at(-1)!) {
                // stack onto this
                break;
            }

            endTimestampStack.pop();
        }

        const durationNs = ((r.timestamp.end - r.timestamp.begin) * 1_000_000_000n) / timestampFrequency;
        const labelText = formatSectionText(r);
        const tooltipText = `${labelText} (${displayNanos(durationNs)})`;
        const left =
            timestampToSecs(r.timestamp.begin - baseTimestamp, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;
        const right =
            timestampToSecs(r.timestamp.end - baseTimestamp, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;
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
            const labelText = markerAddrToName.get(e.markerAddr)!;
            const left =
                timestampToSecs(e.timestamp - baseTimestamp, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;

            return { labelText, left };
        });

    return { barRects: rects, eventLines };
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
