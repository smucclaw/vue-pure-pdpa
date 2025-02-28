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
  prePost: Label | null;
  mark: Ternary;
  children: Q[];
}

export function mkQ(
  shouldView: ShouldView,
  andOr: AndOr,
  prePost: Label | null,
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