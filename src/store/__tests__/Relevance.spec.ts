import { describe, it, expect } from 'vitest'

import { evaluate } from '@ps/AnyAll.Relevance';
import { All, Any, False, Leaf, Marking, Not, Pre, True, Unknown } from '@ps/AnyAll.Types';
import { singleton } from '@ps/Data.Map';

const keyString = 'key';
const keyLeaf = new Leaf('key');
const missingLeaf = new Leaf('missing');


function makeMarking(mark: any) {
  return Marking(singleton(keyString)(mark));
}

function makeAny(marks: string[]) {
  return new Any(new Pre("dummy"), marks.map((k) => new Leaf(k)));
}


function makeAll(marks: string[]) {
  return new All(new Pre("dummy"), marks.map((k) => new Leaf(k)));
}

function makeNot(marks: string) {
  return new Not(new Leaf(marks));
}

describe('evaluate', () => {
  describe('Leaf', () => {
    it('key present in marking and True', () => {
      expect(
        evaluate(makeMarking(True.value))(keyLeaf)
      ).toStrictEqual(
        True.value
      );
    });

    it('key present in marking and False', () => {
      expect(
        evaluate(makeMarking(False.value))(keyLeaf)
      ).toStrictEqual(
        False.value
      );
    });

    it('key missing in marking and True', () => {
      expect(
        evaluate(makeMarking(True.value))(missingLeaf)
      ).toStrictEqual(
        Unknown.value
      );
    });

    it('key missing in marking and False', () => {
      expect(
        evaluate(makeMarking(False.value))(missingLeaf)
      ).toStrictEqual(
        Unknown.value
      );
    });
  })

  describe('Any', () => {
    it('Any and True is present', () => {
      expect(
        evaluate(makeMarking(True.value))(makeAny([keyString, "other"]))
      ).toStrictEqual(
        True.value
      );
    });

    it('Any and only False', () => {
      expect(
        evaluate(makeMarking(False.value))(makeAny([keyString]))
      ).toStrictEqual(
        False.value
      );
    });

    it('Any and missing', () => {
      expect(
        evaluate(makeMarking(False.value))(makeAny(["missing"]))
      ).toStrictEqual(
        Unknown.value
      );
    });
  })

  describe('All', () => {
    it('All and True is present', () => {
      expect(
        evaluate(makeMarking(False.value))(makeAll([keyString, "other"]))
      ).toStrictEqual(
        False.value
      );
    });

    it('All and only True', () => {
      expect(
        evaluate(makeMarking(True.value))(makeAll([keyString]))
      ).toStrictEqual(
        True.value
      );
    });

    it('All and missing', () => {
      expect(
        evaluate(makeMarking(False.value))(makeAll(["missing"]))
      ).toStrictEqual(
        Unknown.value
      );
    });
  })

  describe('Not', () => {
    it('Not and True is present', () => {
      expect(
        evaluate(makeMarking(True.value))(makeNot(keyString))
      ).toStrictEqual(
        False.value
      );
    });

    it('Not and only False', () => {
      expect(
        evaluate(makeMarking(False.value))(makeNot(keyString))
      ).toStrictEqual(
        True.value
      );
    });

    it('Not and missing', () => {
      expect(
        evaluate(makeMarking(False.value))(makeNot("missing"))
      ).toStrictEqual(
        Unknown.value
      );
    });
  })
});