export type Range<T> = {
    readonly begin: T;
    readonly end: T;
};

export function bnRangeSlice(source: Range<bigint>, section: Range<bigint>): Range<bigint> {
    const newBegin = source.begin - section.begin;
    const newEnd = source.end - section.begin;
    const endLimit = section.end - section.begin;

    return {
        begin: newBegin < 0 ? 0n : newBegin,
        end: newEnd < 0 ? 0n : newEnd > endLimit ? endLimit : newEnd,
    };
}

/** lazily initialized value cell */
export class Lazy<T> {
    #cachedValue: T | undefined = undefined;

    constructor(private readonly initializer: () => T) {}

    get value(): T {
        return (this.#cachedValue ??= this.initializer());
    }
}

export function hasValue<T>(value: T | null | undefined): value is NonNullable<T> {
    return value !== undefined && value !== null;
}

export function bnMax(a: bigint, b: bigint): bigint {
    return a < b ? b : a;
}

export function bnMin(a: bigint, b: bigint): bigint {
    return a < b ? a : b;
}

export function assert(condition: boolean, message: string = "Assertion Failure"): void {
    if (!condition) {
        throw new Error(message);
    }
}

export function assertEq<T>(a: T, b: T, message: string = "Assertion Failure"): void {
    if (a !== b) {
        throw new Error(`${message}: ${a} !== ${b}`);
    }
}

export function timestampToSecs(timestamp: bigint, freq: bigint): number {
    return Number(timestamp) / Number(freq);
}
