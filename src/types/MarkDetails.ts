export interface MarkDetails {
    source: string;
    value: boolean | undefined;
}

export function userMark(value: boolean): MarkDetails {
    return {
        source: 'user',
        value: value,
    };
}