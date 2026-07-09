export class PrefixedViewGroup {
    constructor(private readonly prefix: string) {}

    #makeFullID(id: string): string {
        return `${this.prefix}-${id}`;
    }

    view<E = HTMLElement>(id: string): ViewElement<E> {
        return new ViewElement<E>(this.#makeFullID(id));
    }

    queryView<E = HTMLElement>(id: string): E {
        return document.getElementById(this.#makeFullID(id)) as E;
    }
}

export class ViewElement<E = HTMLElement> {
    #cachedRef: E | undefined = undefined;

    constructor(private readonly id: string) {}

    get ref(): E {
        return (this.#cachedRef ??= document.getElementById(this.id) as E);
    }
}
