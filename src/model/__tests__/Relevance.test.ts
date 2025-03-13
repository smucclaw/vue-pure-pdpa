import { evaluate, Marking } from '../Relevance';
import { describe, it, expect } from 'vitest'
import { Ternary } from '../Ternary';
import { Item, createLeaf, createAll, createAny, createNot, Label } from '../Item';

// Constants
export const keyString: string = "key";

export function right(b: Ternary): Marking {
  const map = new Map<string, Ternary>();
  map.set(keyString, b);
  return map;
}

export const keyLeaf: Item = createLeaf(keyString);

export const missingLeaf: Item = createLeaf("missing");

export function any(leafs: Array<string>): Item {
  return createAny(
    { type: "Pre", pre: "dummy" },
    leafs.map(leaf => createLeaf(leaf))
  );
}

export function all(leafs: Array<string>): Item {
  return createAll(
    { type: "Pre", pre: "dummy" },
    leafs.map(leaf => createLeaf(leaf))
  );
}

export function not(leaf: string): Item {
  return createNot(createLeaf(leaf));
}

describe("evaluate", () => {
  describe("leaf", () => {
    describe("key present in marking", () => {
      it("Hard matching right true", () => {
        expect(evaluate(right(Ternary.True), keyLeaf)).toEqual(Ternary.True);
      });
      
      it("Hard matching right false", () => {
        expect(evaluate(right(Ternary.False), keyLeaf)).toEqual(Ternary.False);
      });
    });

    describe("key is not present in marking", () => {
      it("Hard missing right true", () => {
        expect(evaluate(right(Ternary.True), missingLeaf)).toEqual(Ternary.Unknown);
      });
      
      it("Hard missing right false", () => {
        expect(evaluate(right(Ternary.False), missingLeaf)).toEqual(Ternary.Unknown);
      });
    });
  });

  describe("Any", () => {
    it("true present", () => {
      expect(evaluate(right(Ternary.True), any(["key", "run"]))).toEqual(Ternary.True);
    });
    
    it("all false", () => {
      expect(evaluate(right(Ternary.False), any(["key"]))).toEqual(Ternary.False);
    });
    
    it("missing key", () => {
      expect(evaluate(right(Ternary.False), any(["missing"]))).toEqual(Ternary.Unknown);
    });
  });

  describe("All", () => {
    it("all true", () => {
      expect(evaluate(right(Ternary.True), all(["key"]))).toEqual(Ternary.True);
    });
    
    it("false present", () => {
      expect(evaluate(right(Ternary.False), all(["key", "run"]))).toEqual(Ternary.False);
    });
    
    it("missing key", () => {
      expect(evaluate(right(Ternary.False), all(["missing"]))).toEqual(Ternary.Unknown);
    });
  });

  describe("Not", () => {
    it("not true", () => {
      expect(evaluate(right(Ternary.True), not("key"))).toEqual(Ternary.False);
    });
    
    it("not false", () => {
      expect(evaluate(right(Ternary.False), not("key"))).toEqual(Ternary.True);
    });
    
    it("missing key", () => {
      expect(evaluate(right(Ternary.False), not("missing"))).toEqual(Ternary.Unknown);
    });
  });
});