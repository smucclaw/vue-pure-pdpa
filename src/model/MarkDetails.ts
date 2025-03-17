import { Ternary, ternaryFromString } from "./Ternary";

export interface MarkDetails {
    source: string;
    value: Ternary;
}

export function userMark(value: string): MarkDetails {
    return {
        source: 'user',
        value: ternaryFromString(value)
    };
}