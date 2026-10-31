export declare class TypedEventTarget<Events extends Record<string, unknown>> extends EventTarget {
    addEventListener<T extends keyof Events>(
        type: T,
        callback: (this: this, e: CustomEvent<Events[T]>) => void,
        options?: AddEventListenerOptions | boolean,
    ): void;
    addEventListener(...args: Parameters<EventTarget["addEventListener"]>): void;
}
