export class PrefixedViewGroup {
    constructor(private readonly prefix: string) {}

    view<E = HTMLElement>(id: string): ViewElement<E> {
        return new ViewElement<E>(`${this.prefix}.${id}`);
    }
}

export class ViewElement<E = HTMLElement> {
    #cachedRef: E | undefined = undefined;

    constructor(private readonly id: string) {}

    get ref(): E {
        return (this.#cachedRef ??= document.getElementById(this.id) as E);
    }
}
