import { Marking, relevant } from '../Relevance';
import { describe, it, expect } from 'vitest'
import { Ternary } from '../Ternary';
import { Item } from '../Item';
import { ShouldView } from '../Interview';

// Helper constants and functions
export const keyString: string = "key";

export function right(b: Ternary): Marking {
  const map = new Map<string, Ternary>();
  map.set(keyString, b);
  return map;
}

export const keyLeaf: Item = {
  type: 'Leaf',
  value: keyString
};

export const missingLeaf: Item = {
  type: 'Leaf',
  value: "missing"
};

export function any(leafs: Array<string>): Item {
  return {
    type: 'Any',
    label: { type: 'Pre', value: "dummy" },
    children: leafs.map(leaf => ({ type: 'Leaf', value: leaf }))
  };
}

export function all(leafs: Array<string>): Item {
  return {
    type: 'All',
    label: { type: 'Pre', value: "dummy" },
    children: leafs.map(leaf => ({ type: 'Leaf', value: leaf }))
  };
}

export function not(leaf: string): Item {
  return {
    type: 'Not',
    value: { type: 'Leaf', value: leaf }
  };
}

export const emptyMarking: Marking = new Map<string, Ternary>();

export const example1: Item = {
  type: 'Leaf',
  value: "single"
};

export const myq: Q = {
  andOr: { type: 'Simply', value: "single" },
  children: [],
  mark: Ternary.Unknown,
  prePost: null,
  shouldView: ShouldView.Ask
};

export function getShouldView(q: Q): ShouldView {
  return q.shouldView;
}

export function derive1(initVis: ShouldView): ShouldView {
  return initVis !== ShouldView.Hide ? initVis : ShouldView.Hide;
}

describe("hide view", () => {
  describe("Hard", () => {
    it("Self Just T / Parent Just T", () => {
      const qq = relevant(right(Ternary.True), Ternary.True, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.View);
    });
    
    it("Self Just F / Parent Just T", () => {
      const qq = relevant(right(Ternary.False), Ternary.True, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.Hide);
    });
    
    it("Self Nothing / Parent Just T", () => {
      const qq = relevant(emptyMarking, Ternary.True, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.Hide);
    });
    
    it("Self Just T / Parent Just F", () => {
      const qq = relevant(right(Ternary.True), Ternary.False, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.Hide);
    });
    
    it("Self Just F / Parent Just F", () => {
      const qq = relevant(right(Ternary.False), Ternary.False, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.View);
    });
    
    it("Self Nothing / Parent Just F", () => {
      const qq = relevant(emptyMarking, Ternary.False, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.Hide);
    });
    
    it("Self Just T / Parent Nothing", () => {
      const qq = relevant(right(Ternary.True), Ternary.Unknown, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.View);
    });
    
    it("Self Just F / Parent Nothing", () => {
      const qq = relevant(right(Ternary.False), Ternary.Unknown, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.View);
    });
    
    it("Self Nothing / Parent Nothing", () => {
      const qq = relevant(emptyMarking, Ternary.Unknown, keyLeaf);
      expect(getShouldView(qq)).toEqual(ShouldView.Ask);
    });
  });
});