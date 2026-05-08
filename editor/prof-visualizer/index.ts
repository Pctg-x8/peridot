import { loadBin, type EventMarker, type SectionBeginMarker, type SectionEndMarker } from "./src/binloader";
import { bnMin, hasValue } from "./src/utils";
import { PrefixedViewGroup, ViewElement } from "./src/viewHelper";

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
                binMetadata.markerAddrToName
            );
            console.log(timelineChartModel);

            const d = new DocumentFragment();
            for (const r of timelineChartModel.barRects) {
                const hue =
                    r.labelText
                        .split("")
                        .map((c) => c.charCodeAt(0) * 7 * 7)
                        .reduce((a, b) => a + b, 0) % 360;

                const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
                const e = document.createElementNS("http://www.w3.org/2000/svg", "rect");
                e.setAttribute("x", r.left.toString());
                e.setAttribute("y", r.top.toString());
                e.setAttribute("width", r.width.toString());
                e.setAttribute("height", r.height.toString());
                e.style.fill = `oklch(100% 0.5 ${hue})`;
                e.setAttribute("stroke", "transparent");
                e.setAttribute("stroke-width", "0");
                const t = document.createElementNS("http://www.w3.org/2000/svg", "title");
                t.textContent = r.labelText;

                g.appendChild(e);
                g.appendChild(t);
                d.appendChild(g);
            }

            const w = timelineChartModel.barRects.reduce((a, b) => Math.max(a, b.left + b.width), 0);
            const h = timelineChartModel.barRects.reduce((a, b) => Math.max(a, b.top + b.height), 0);
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
    readonly timestamp: Range<bigint>;
};

function buildSectionRanges(
    markers: (SectionBeginMarker | SectionEndMarker)[],
    markerAddrToName: Map<bigint, string>
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
                timestamp: {
                    begin: v.begin?.timestamp ?? 0n,
                    end: v.end?.timestamp ?? maxTimestamp,
                },
            };
        })
        .toArray();
}

const TIMELINE_CHART_BAR_THICKNESS: number = 12.0;
const TIMELINE_CHART_WIDTH_PER_SEC: number = 128.0 * 10.0;

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
    markerAddrToName: Map<bigint, string>
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

        const labelText = r.markerName;
        const tooltipText = labelText;
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
        .map((e) => {
            const labelText = markerAddrToName.get(e.markerAddr)!;
            const left =
                timestampToSecs(e.timestamp - baseTimestamp, timestampFrequency) * TIMELINE_CHART_WIDTH_PER_SEC;

            return { labelText, left };
        });

    return { barRects: rects, eventLines };
}
