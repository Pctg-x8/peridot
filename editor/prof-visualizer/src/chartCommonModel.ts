import type { HierarchySectionRange, SectionRange } from "./app";
import type { EventMarker } from "./binloader";
import { timestampToSecs, type Range } from "./utils";

export type BarRect = {
    readonly labelText: string;
    readonly tooltipText: string;
    readonly left: number;
    readonly top: number;
    readonly width: number;
    readonly height: number;
};

export type TimelineChartDesignMetrics = {
    readonly widthPerSec: number;
    readonly barThickness: number;
};
export type TimelineChartModel = {
    readonly barRects: BarRect[];
    readonly eventLines: EventLine[];
};
export function buildTimelineChartModel(
    sectionRanges: SectionRange[],
    events: EventMarker[],
    timestampRange: Range<bigint>,
    timestampFrequency: bigint,
    markerAddrToName: Map<bigint, string>,
    designMetrics: TimelineChartDesignMetrics,
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

        if (r.timestamp.end < timestampRange.begin || timestampRange.end < r.timestamp.begin) {
            // completely out of range
            continue;
        }

        const durationNs = ((r.timestamp.end - r.timestamp.begin) * 1_000_000_000n) / timestampFrequency;
        const labelText = formatSectionText(r);
        const tooltipText = `${labelText} (${displayNanos(durationNs)})`;
        const left =
            timestampToSecs(r.timestamp.begin - timestampRange.begin, timestampFrequency) * designMetrics.widthPerSec;
        const right =
            timestampToSecs(r.timestamp.end - timestampRange.begin, timestampFrequency) * designMetrics.widthPerSec;
        rects.push({
            labelText,
            tooltipText,
            left,
            top: endTimestampStack.length * designMetrics.barThickness,
            width: right - left,
            height: designMetrics.barThickness,
        });
        endTimestampStack.push(r.timestamp.end);
    }

    const eventLines = events
        .toSorted((a, b) => Number(a.timestamp - b.timestamp))
        .map(e => {
            if (e.timestamp < timestampRange.begin || timestampRange.end < e.timestamp) {
                // out of range
                return null;
            }

            const labelText = markerAddrToName.get(e.markerAddr)!;
            const left =
                timestampToSecs(e.timestamp - timestampRange.begin, timestampFrequency) * designMetrics.widthPerSec;

            return { labelText, left };
        })
        .filter(x => x !== null);

    return { barRects: rects, eventLines };
}
export function buildTimelineChartModelFromHierarchyData(
    rootRanges: HierarchySectionRange[],
    events: EventMarker[],
    timestampRange: Range<bigint>,
    timestampFrequency: bigint,
    markerAddrToName: Map<bigint, string>,
    designMetrics: TimelineChartDesignMetrics,
): TimelineChartModel {
    const rects: BarRect[] = [];
    const processStack: (readonly [HierarchySectionRange, number])[] = [];
    processStack.push(...rootRanges.map(r => [r, 0] as const));
    while (processStack.length > 0) {
        const [r, depth] = processStack.pop()!;

        if (r.range.timestamp.end < timestampRange.begin || timestampRange.end < r.range.timestamp.begin) {
            // completely out of range
            continue;
        }

        const durationNs = ((r.range.timestamp.end - r.range.timestamp.begin) * 1_000_000_000n) / timestampFrequency;
        const childrenDurationNsTotal = r.children.reduce(
            (a, c) => a + ((c.range.timestamp.end - c.range.timestamp.begin) * 1_000_000_000n) / timestampFrequency,
            0n,
        );
        const selfDurationNs = durationNs - childrenDurationNsTotal;
        const labelText = formatSectionText(r.range);
        const tooltipText = `${labelText} (${displayNanos(durationNs)} self=${displayNanos(selfDurationNs)})`;
        const left =
            timestampToSecs(r.range.timestamp.begin - timestampRange.begin, timestampFrequency) *
            designMetrics.widthPerSec;
        const right =
            timestampToSecs(r.range.timestamp.end - timestampRange.begin, timestampFrequency) *
            designMetrics.widthPerSec;
        rects.push({
            labelText,
            tooltipText,
            left,
            top: depth * designMetrics.barThickness,
            width: right - left,
            height: designMetrics.barThickness,
        });

        processStack.push(...r.children.map(r => [r, depth + 1] as const));
    }

    const eventLines = events
        .toSorted((a, b) => Number(a.timestamp - b.timestamp))
        .map(e => {
            if (e.timestamp < timestampRange.begin || timestampRange.end < e.timestamp) {
                // out of range
                return null;
            }

            const labelText = markerAddrToName.get(e.markerAddr)!;
            const left =
                timestampToSecs(e.timestamp - timestampRange.begin, timestampFrequency) * designMetrics.widthPerSec;

            return { labelText, left };
        })
        .filter(x => x !== null);

    return { barRects: rects, eventLines };
}
export function timelineChartHeight(model: TimelineChartModel): number {
    return model.barRects.map(x => x.top + x.height).reduce((a, b) => Math.max(a, b), 0);
}
export type InstantiationOptions = {
    readonly horizontalScale: number;
};
export function instantiateTimelineChart(
    rootNode: Node,
    model: TimelineChartModel,
    yTop: number,
    eventsYTop: number,
    options: InstantiationOptions,
) {
    let barRectId = 0;
    for (const r of model.barRects) {
        const top = yTop + r.top;

        const clip = document.createElementNS("http://www.w3.org/2000/svg", "clipPath");
        const clipId = (clip.id = `barRectClip-${barRectId}`);
        const rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        rect.setAttribute("x", (r.left * options.horizontalScale).toString());
        rect.setAttribute("y", top.toString());
        rect.setAttribute("width", (r.width * options.horizontalScale).toString());
        rect.setAttribute("height", r.height.toString());
        clip.appendChild(rect);
        rootNode.appendChild(clip);

        const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
        const e = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        e.setAttribute("x", (r.left * options.horizontalScale).toString());
        e.setAttribute("y", top.toString());
        e.setAttribute("width", (r.width * options.horizontalScale).toString());
        e.setAttribute("height", r.height.toString());
        e.style.fill = barColor(r);
        e.setAttribute("stroke", "transparent");
        e.setAttribute("stroke-width", "0");
        const tx = document.createElementNS("http://www.w3.org/2000/svg", "text");
        tx.textContent = r.labelText;
        tx.setAttribute("x", (r.left * options.horizontalScale).toString());
        tx.setAttribute("y", (top + r.height * 0.5).toString());
        tx.setAttribute("clip-path", `url(#${clipId})`);
        tx.setAttribute("dominant-baseline", "middle");
        const t = document.createElementNS("http://www.w3.org/2000/svg", "title");
        t.textContent = r.tooltipText;

        g.appendChild(e);
        g.appendChild(tx);
        g.appendChild(t);
        rootNode.appendChild(g);
        barRectId += 1;
    }

    for (const l of model.eventLines) {
        const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
        text.textContent = l.labelText;
        text.setAttribute("x", (l.left * options.horizontalScale).toString());
        text.setAttribute("y", eventsYTop.toString());
        text.setAttribute("dominant-baseline", "text-top");
        text.classList.add("eventLine");

        const line = document.createElementNS("http://www.w3.org/2000/svg", "line");
        line.setAttribute("x1", (l.left * options.horizontalScale).toString());
        line.setAttribute("x2", (l.left * options.horizontalScale).toString());
        line.setAttribute("y1", eventsYTop.toString());
        line.setAttribute("y2", "100%");
        line.setAttribute("stroke-width", "1");
        line.setAttribute("stroke", "#999");

        rootNode.appendChild(text);
        rootNode.appendChild(line);
    }
}

export function barColor(bar: BarRect): string {
    const hue =
        bar.labelText
            .split("")
            .map(c => c.charCodeAt(0) * 7)
            .reduce((a, b) => a + b, 0) % 360;

    return `oklch(100% 0.25 ${hue})`;
}

export type EventLine = {
    readonly labelText: string;
    readonly left: number;
};

export type ChartPoint = {
    readonly x: number;
    readonly y: number;
    readonly tooltipText: string;
};

export function formatSectionText(section: SectionRange): string {
    let text = section.markerName;
    if (section.auxData.length > 0) {
        text += `: ${section.auxData.join(", ")}`;
    }

    return text;
}

export function displayNanos(ns: bigint): string {
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

export function displayByteSize(bytes: bigint): string {
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
