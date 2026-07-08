export type Range<T> = {
    readonly begin: T;
    readonly end: T;
};

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
