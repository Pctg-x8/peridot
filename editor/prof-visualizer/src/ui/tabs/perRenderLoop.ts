import { Application } from "../../app";
import {
    buildTimelineChartModel,
    instantiateTimelineChart,
    timelineChartHeight,
    type TimelineChartDesignMetrics,
    type TimelineChartModel,
} from "../../chartCommonModel";
import { hasValue, timestampToSecs, type Range } from "../../utils";
import { PrefixedViewGroup, ViewElement } from "../../viewHelper";

export class Presenter {
    static readonly #ViewGroup = new PrefixedViewGroup("PerRenderLoopTab");

    readonly #controls = Presenter.#ViewGroup.view<HTMLElement>("Controls");
    readonly #currentFrameNumberText = Presenter.#ViewGroup.view<HTMLSpanElement>("CurrentFrameNumber");
    readonly #totalFrameNumberText = Presenter.#ViewGroup.view<HTMLSpanElement>("MaxFrameNumber");
    readonly #frameSlider = Presenter.#ViewGroup.view<HTMLInputElement>("FrameSlider");
    readonly #prevFrameButton = Presenter.#ViewGroup.view<HTMLButtonElement>("PrevFrameButton");
    readonly #nextFrameButton = Presenter.#ViewGroup.view<HTMLButtonElement>("NextFrameButton");

    readonly #chartSurface = new ViewElement<SVGGElement>("ChartSurface");
    readonly #chartContainer = new ViewElement<SVGGElement>("ChartContainer");

    #isActiveView: boolean = false;
    #viewActivated: boolean = false;
    #sourceDataChanged: boolean = false;
    #frameNumberUpdated: boolean = true; // initial dirty
    #currentFrameNumber: number = 0;
    #renderLoopTimestampRanges: Range<bigint>[] = [];
    #timelineChartModel: TimelineChartModel | null = null;
    #timestampRange: Range<bigint> = { begin: 0n, end: 0n };

    launch() {
        this.#frameSlider.ref.addEventListener("change", () => {
            this.#currentFrameNumber = Number(this.#frameSlider.ref.value);
            this.#frameNumberUpdated = true;
            this.#performAtomic();
        });
        this.#nextFrameButton.ref.addEventListener("click", () => {
            this.#currentFrameNumber = Math.min(
                this.#currentFrameNumber + 1,
                this.#renderLoopTimestampRanges.length - 1,
            );
            this.#frameNumberUpdated = true;
            this.#performAtomic();
        });
        this.#prevFrameButton.ref.addEventListener("click", () => {
            this.#currentFrameNumber = Math.max(this.#currentFrameNumber - 1, 0);
            this.#frameNumberUpdated = true;
            this.#performAtomic();
        });

        this.#isActiveView = Application.instance.currentTab == "PerRenderLoop";
        Application.instance.addEventListener("tabSwitched", t => {
            this.#isActiveView = t.detail == "PerRenderLoop";
            this.#viewActivated = this.#isActiveView;
        });
        this.#sourceDataChanged = false;
        Application.instance.addEventListener("sourceDataChanged", () => {
            this.#sourceDataChanged = true;
        });
        Application.instance.addEventListener("performAtomic", () => {
            this.#performAtomic();
        });
    }

    #performAtomic() {
        if (!this.#isActiveView) {
            // suspend until active
            this.#controls.ref.classList.add("hidden");
            return;
        }

        this.#controls.ref.classList.remove("hidden");

        let shouldRenderChart = false;
        if (this.#viewActivated) {
            shouldRenderChart = true;
            this.#viewActivated = false;
        }

        if (this.#sourceDataChanged) {
            this.#renderLoopTimestampRanges = Application.instance.sectionRanges
                .filter(r => r.markerName === "RenderLoop")
                .map(r => r.timestamp);
            this.#currentFrameNumber = 0;
            this.#frameNumberUpdated = true;

            this.#sourceDataChanged = false;
            shouldRenderChart = true;
        }

        if (this.#frameNumberUpdated) {
            this.#totalFrameNumberText.ref.textContent = this.#renderLoopTimestampRanges.length.toString();
            this.#currentFrameNumberText.ref.textContent = this.#currentFrameNumber.toString();
            this.#frameSlider.ref.max = (this.#renderLoopTimestampRanges.length - 1).toString();
            this.#frameSlider.ref.value = this.#currentFrameNumber.toString();
            this.#prevFrameButton.ref.disabled = this.#currentFrameNumber <= 0;
            this.#nextFrameButton.ref.disabled = this.#currentFrameNumber >= this.#renderLoopTimestampRanges.length - 1;

            const binMetadata = Application.instance.currentBinMetadata;
            if (hasValue(binMetadata)) {
                this.#timelineChartModel = buildTimelineChartModel(
                    Application.instance.sectionRanges,
                    Application.instance.events,
                    this.#renderLoopTimestampRanges[this.#currentFrameNumber],
                    Application.instance.currentBinMetadata!.timestampFrequency,
                    Application.instance.currentBinMetadata!.markerAddrToName,
                    TIMELINE_CHART_DESIGN_METRICS,
                );
                this.#timestampRange = this.#renderLoopTimestampRanges[this.#currentFrameNumber];
            } else {
                this.#timelineChartModel = null;
            }

            this.#frameNumberUpdated = false;
            shouldRenderChart = true;
        }

        if (shouldRenderChart) {
            this.#renderChart();
        }
    }

    #renderChart() {
        const binMetadata = Application.instance.currentBinMetadata;
        if (!hasValue(binMetadata) || !hasValue(this.#timelineChartModel)) {
            // not loaded
            return;
        }

        // TODO: adjustable
        const horizontalScale = 1.0;

        const timelineChartContentHeight = timelineChartHeight(this.#timelineChartModel);

        const d = new DocumentFragment();

        instantiateTimelineChart(d, this.#timelineChartModel, TIMELINE_CHART_TOP_MARGIN, 0, { horizontalScale });

        const w =
            timestampToSecs(this.#timestampRange.end - this.#timestampRange.begin, binMetadata.timestampFrequency) *
            TIMELINE_CHART_DESIGN_METRICS.widthPerSec *
            horizontalScale;
        const h = TIMELINE_CHART_TOP_MARGIN + timelineChartContentHeight;
        this.#chartSurface.ref.setAttribute("width", (w * window.devicePixelRatio).toString());
        this.#chartSurface.ref.setAttribute("height", (h * window.devicePixelRatio).toString());
        this.#chartSurface.ref.setAttribute("viewBox", `0 0 ${w} ${h}`);

        this.#chartContainer.ref.replaceChildren(d);
    }
}

const TIMELINE_CHART_DESIGN_METRICS: TimelineChartDesignMetrics = {
    widthPerSec: 128.0 * 500.0,
    barThickness: 12.0,
};
const TIMELINE_CHART_TOP_MARGIN: number = 120.0;
