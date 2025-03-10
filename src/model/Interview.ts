import { Label } from './Item';
import { Ternary } from './Ternary';


export enum ShouldView {
  View = 'View',
  Hide = 'Hide',
  Ask = 'Ask'
}

export abstract class AndOr {
}

export class And extends AndOr {
  readonly type = 'And';
}

export class Or extends AndOr {
  readonly type = 'Or';
}

export class Simply extends AndOr {
  readonly type = 'Simply';
  constructor(public readonly value: string) {
    super();
  }
}

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
    mark: q.mark,
    andOr: encodeAndOrArgo(q.andOr, q.children)
  };
}

function encodeAndOrArgo(andOr: AndOr, children: Q[]): object {
  if (andOr instanceof Simply) {
    return {
      tag: "Leaf",
      contents: andOr.value,
      nl: {}
    };
  } else if (andOr instanceof And) {
    console.log('encodeAndOrArgo', andOr, children);

    return {
      tag: "All",
      children: children.map(encodeJsonQ),
      nl: {}
    };
  } else if (andOr instanceof Or) {
    return {
      tag: "Any",
      children: children.map(encodeJsonQ),
      nl: {}
    };
  }
}

function encodePrePostArgo(prePost?: Label): object {
  return prePost ? { value: prePost } : {};
}