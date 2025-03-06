import { Marking, relevant } from '../Relevance';
import { describe, it, expect } from 'vitest'
import { Ternary } from '../Ternary';
import { AllItem, AnyItem, Item, LeafItem, NotItem, PreLabel } from '../Item';
import { Q, ShouldView } from '../Interview';

// Helper constants and functions
export const keyString: string = "key";

export function right(b: Ternary): Marking {
  const map = new Map<string, Ternary>();
  map.set(keyString, b);
  return map;
}

export const keyLeaf: Item = new LeafItem(keyString);

export const missingLeaf: Item = new LeafItem("missing");


export function any(leafs: Array<string>): Item {
  return new AnyItem(
    new PreLabel("dummy"),
    leafs.map(leaf => new LeafItem(leaf))
  );
}

export function all(leafs: Array<string>): Item {
  return new AllItem(
    new PreLabel("dummy"),
    leafs.map(leaf => new LeafItem(leaf))
  );
}

export function not(leaf: string): Item {
  return new NotItem(
    new LeafItem(leaf)
  );
}

export const emptyMarking: Marking = new Map<string, Ternary>();

export const example1: Item = new LeafItem("single");

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