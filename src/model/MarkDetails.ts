import { Ternary, ternaryFromString } from "./Ternary";

export class MarkDetails {
    source: string;
    value: Ternary;

    constructor(source: string, value: Ternary) {
        this.source = source;
        this.value = value;
    }
}

export function userMark(value: string): MarkDetails {
    return new MarkDetails('user', ternaryFromString(value));
}