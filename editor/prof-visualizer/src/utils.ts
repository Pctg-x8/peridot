export function hasValue<T>(value: T | null | undefined): value is NonNullable<T> {
    return value !== undefined && value !== null;
}

export function bnMin(a: bigint, b: bigint): bigint {
    return a < b ? a : b;
}
