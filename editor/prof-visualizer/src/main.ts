import { Application, type SectionRange, type ViewTab } from "./app";
import { type MemoryStatsMarker } from "./binloader";
import {
    buildTimelineChartModel,
    displayByteSize,
    instantiateTimelineChart,
    timelineChartHeight,
    type ChartPoint,
    type TimelineChartDesignMetrics,
    type TimelineChartModel,
} from "./chartCommonModel";
import { hasValue, Lazy, timestampToSecs, type Range } from "./utils";
import { PrefixedViewGroup, ViewElement } from "./viewHelper";
import * as PerRenderLoopTab from "./ui/tabs/perRenderLoop";

document.addEventListener("DOMContentLoaded", () => {
    new HeaderPresenter().launch();
    new PerRenderLoopTab.Presenter().launch();

    Application.instance.sync();
});

class HeaderPresenter {
    static readonly #ViewGroup = new PrefixedViewGroup("Header");

    readonly #inputFileView = HeaderPresenter.#ViewGroup.view<HTMLInputElement>("InputFile");
    readonly #loadButtonView = HeaderPresenter.#ViewGroup.view<HTMLButtonElement>("LoadButton");
    readonly #horizontalScaleInputView = HeaderPresenter.#ViewGroup.view<HTMLInputElement>("HorizontalScaleInput");
    readonly #chartSurface = new ViewElement<SVGGElement>("ChartSurface");
    readonly #chartContainer = new ViewElement<SVGGElement>("ChartContainer");
    readonly #tabViewSelector = new Lazy(
        () =>
            HeaderPresenter.#ViewGroup
                .queryView<HTMLFormElement>("TabViewSelector")
                .elements.namedItem("items")! as RadioNodeList,
    );

    #timelineChartModel: TimelineChartModel | null = null;
    #memoryChartModel: MemoryChartModel | null = null;
    #timestampRange: Range<bigint> = { begin: 0n, end: 0n };

    static #modelToViewValue(model: ViewTab): string {
        switch (model) {
            case "Main":
                return "main";
            case "PerRenderLoop":
                return "perRenderLoop";
        }
    }

    static #viewValueToModel(value: string): ViewTab {
        switch (value) {
            case "main":
                return "Main";
            case "perRenderLoop":
                return "PerRenderLoop";
            default:
                throw new Error(`Unhandled tab value: ${value}`);
        }
    }

    launch() {
        this.#loadButtonView.ref.addEventListener("click", () => {
            const file = this.#inputFileView.ref.files?.item(0);
            if (!hasValue(file)) {
                alert("ファイルが選択されていません");
                return;
            }

            Application.instance.load(file);
        });

        this.#tabViewSelector.value.value = HeaderPresenter.#modelToViewValue(Application.instance.currentTab);
        this.#tabViewSelector.value.forEach(e => {
            e.addEventListener("change", function () {
                Application.instance.switchTab(HeaderPresenter.#viewValueToModel(this.value));
            });
        });

        this.#horizontalScaleInputView.ref.value = "1";
        this.#horizontalScaleInputView.ref.addEventListener("change", () => {
            this.#renderChart();
        });

        let isActiveView = Application.instance.currentTab === "Main";
        let viewActivated = false;
        Application.instance.addEventListener("tabSwitched", t => {
            isActiveView = t.detail === "Main";
            viewActivated = isActiveView;
        });
        let sourceDataChanged = false;
        Application.instance.addEventListener("sourceDataChanged", () => {
            sourceDataChanged = true;
        });
        Application.instance.addEventListener("performAtomic", () => {
            if (!isActiveView) {
                // suspending
                return;
            }

            var needsRender = false;
            if (viewActivated) {
                needsRender = true;
                viewActivated = false;
            }

            if (sourceDataChanged) {
                const binMetadata = Application.instance.currentBinMetadata;
                if (!hasValue(binMetadata)) {
                    this.#timelineChartModel = null;
                    this.#memoryChartModel = null;
                } else {
                    this.#timestampRange = Application.instance.computeChartTimestampRange();

                    this.#timelineChartModel = buildTimelineChartModel(
                        Application.instance.sectionRanges,
                        Application.instance.events,
                        this.#timestampRange,
                        binMetadata.timestampFrequency,
                        binMetadata.markerAddrToName,
                        TIMELINE_CHART_DESIGN_METRICS,
                    );
                    console.log(this.#timelineChartModel);
                    this.#memoryChartModel = buildMemoryChartModel(
                        Application.instance.memoryStats,
                        this.#timestampRange,
                        binMetadata.timestampFrequency,
                    );
                    console.log(this.#memoryChartModel);
                }

                needsRender = true;
                sourceDataChanged = false;
            }

            if (needsRender) {
                this.#renderChart();
            }
        });
    }

    #renderChart() {
        const binMetadata = Application.instance.currentBinMetadata;
        if (!hasValue(binMetadata)) {
            // not loaded
            return;
        }

        if (!hasValue(this.#timelineChartModel) || !hasValue(this.#memoryChartModel)) {
            return;
        }

        const horizontalScale1 = Number.parseFloat(this.#horizontalScaleInputView.ref.value);
        const horizontalScale = isNaN(horizontalScale1) ? 1.0 : horizontalScale1;

        // 最大値よりちょっと大きめに取る
        const lineChartContentHeight = Math.max(
            this.#memoryChartModel.totalResident
                .map(x => x.y + 500.0 * MEMORY_CHART_HEIGHT_PER_BYTES)
                .reduce((a, b) => Math.max(a, b), TIMELINE_CHART_TOP_MARGIN),
            this.#memoryChartModel.totalReserved
                .map(x => x.y + 500.0 * MEMORY_CHART_HEIGHT_PER_BYTES)
                .reduce((a, b) => Math.max(a, b), TIMELINE_CHART_TOP_MARGIN),
        );
        const timelineChartContentHeight = timelineChartHeight(this.#timelineChartModel);

        const d = new DocumentFragment();

        const memoryTotalResidentLines = document.createElementNS("http://www.w3.org/2000/svg", "polyline");
        memoryTotalResidentLines.setAttribute(
            "points",
            this.#memoryChartModel.totalResident.map(x => `${x.x},${lineChartContentHeight - x.y}`).join(" "),
        );
        memoryTotalResidentLines.setAttribute("stroke-width", "1");
        memoryTotalResidentLines.setAttribute("stroke", "#ccc");
        memoryTotalResidentLines.setAttribute("fill", "transparent");
        d.appendChild(memoryTotalResidentLines);
        for (const p of this.#memoryChartModel.totalResident) {
            const g = document.createElementNS("http://www.w3.org/2000/svg", "g");

            const point = document.createElementNS("http://www.w3.org/2000/svg", "circle");
            point.setAttribute("cx", (p.x * horizontalScale).toString());
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
            this.#memoryChartModel.totalReserved.map(x => `${x.x},${lineChartContentHeight - x.y}`).join(" "),
        );
        memoryTotalReservedLines.setAttribute("stroke-width", "1");
        memoryTotalReservedLines.setAttribute("stroke", "#ccc");
        memoryTotalReservedLines.setAttribute("fill", "transparent");
        d.appendChild(memoryTotalReservedLines);
        for (const p of this.#memoryChartModel.totalReserved) {
            const g = document.createElementNS("http://www.w3.org/2000/svg", "g");

            const point = document.createElementNS("http://www.w3.org/2000/svg", "circle");
            point.setAttribute("cx", (p.x * horizontalScale).toString());
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
            this.#memoryChartModel.totalPrivateResident.map(x => `${x.x},${lineChartContentHeight - x.y}`).join(" "),
        );
        memoryTotalPrivateResidentLines.setAttribute("stroke-width", "1");
        memoryTotalPrivateResidentLines.setAttribute("stroke", "#ccc");
        memoryTotalPrivateResidentLines.setAttribute("fill", "transparent");
        d.appendChild(memoryTotalPrivateResidentLines);
        for (const p of this.#memoryChartModel.totalPrivateResident) {
            const g = document.createElementNS("http://www.w3.org/2000/svg", "g");

            const point = document.createElementNS("http://www.w3.org/2000/svg", "circle");
            point.setAttribute("cx", (p.x * horizontalScale).toString());
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
        instantiateTimelineChart(d, this.#timelineChartModel, lineChartContentHeight, 0, { horizontalScale });

        const w =
            timestampToSecs(this.#timestampRange.end - this.#timestampRange.begin, binMetadata.timestampFrequency) *
            TIMELINE_CHART_DESIGN_METRICS.widthPerSec *
            horizontalScale;
        const h = lineChartContentHeight + timelineChartContentHeight;
        this.#chartSurface.ref.setAttribute("width", (w * window.devicePixelRatio).toString());
        this.#chartSurface.ref.setAttribute("height", (h * window.devicePixelRatio).toString());
        this.#chartSurface.ref.setAttribute("viewBox", `0 0 ${w} ${h}`);

        this.#chartContainer.ref.replaceChildren(d);
    }
}

const TIMELINE_CHART_DESIGN_METRICS: TimelineChartDesignMetrics = {
    widthPerSec: 128.0 * 50.0,
    barThickness: 12.0,
};
const TIMELINE_CHART_TOP_MARGIN: number = 120.0;

const MEMORY_CHART_HEIGHT_PER_BYTES: number = 1.0 / 1_000_000.0;

type MemoryChartModel = {
    readonly totalResident: ChartPoint[];
    readonly totalReserved: ChartPoint[];
    readonly totalPrivateResident: ChartPoint[];
};
export function buildMemoryChartModel(
    memoryStats: MemoryStatsMarker[],
    timestampRange: Range<bigint>,
    timestampFrequency: bigint,
): MemoryChartModel {
    const totalResident = [];
    const totalReserved = [];
    const totalPrivateResident = [];

    let lastBeyond = false;
    for (const stat of memoryStats.toSorted((a, b) => Number(a.timestamp - b.timestamp))) {
        const past = stat.timestamp < timestampRange.begin;
        const beyond = stat.timestamp >= timestampRange.end;

        const x =
            timestampToSecs(stat.timestamp - timestampRange.begin, timestampFrequency) *
            TIMELINE_CHART_DESIGN_METRICS.widthPerSec;

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
