import { encodePrePostArgo, Label } from './Item';
import { Ternary, ternary2string } from './Ternary';

export enum ShouldView {
  View = 'View',
  Hide = 'Hide',
  Ask = 'Ask'
}

export type AndOr =
  | { type: 'And' }
  | { type: 'Or' }
  | { type: 'Simply'; value: string };

export interface Q {
  shouldView: ShouldView;
  andOr: AndOr;
  prePost: Label | undefined;
  mark: Ternary;
  children: Q[];
}

export function mkQ(
  shouldView: ShouldView,
  andOr: AndOr,
  prePost: Label | undefined,
  mark: Ternary,
  children: Q[]
): Q {
  return {
    shouldView,
    andOr,
    prePost,
    mark,
    children
  };
}

export function shouldViewToString(sv: ShouldView): string {
  return sv;
}

export function encodeJsonQ(q: Q): object {
  return {
    shouldView: q.shouldView,
    prePost: encodePrePostArgo(q.prePost),
    mark: { source: "user", value: ternary2string(q.mark) },
    andOr: encodeAndOrArgo(q.andOr, q.children)
  };
}

function encodeAndOrArgo(andOr: AndOr, children: Q[]): object {
  switch (andOr.type) {
    case 'Simply':
      return {
        tag: "Leaf",
        contents: andOr.value,
        nl: {}
      };
    case 'And':
      return {
        tag: "All",
        children: children.map(encodeJsonQ),
        nl: {}
      };
    case 'Or':
      return {
        tag: "Any",
        children: children.map(encodeJsonQ),
        nl: {}
      };
  }
}