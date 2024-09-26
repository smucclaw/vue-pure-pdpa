import { describe, it, expect } from 'vitest'

import { evaluate } from '@ps/AnyAll.Relevance';
import { False, Leaf, Marking, True } from '@ps/AnyAll.Types';
import { singleton } from '@ps/Data.Map';

const keyString = 'key';
const keyLeaf = new Leaf('key');

function makeMarking(mark: any) {
  return Marking(singleton(keyString)(mark));
}

describe('evaluate', () => {
    it('right key present in marking and True', () => {
      expect(
        evaluate(makeMarking(True))(keyLeaf)
      ).toEqual(
        True
      );
    });

    it('right key present in marking and False', () => {
      expect(
        evaluate(makeMarking(False))(keyLeaf)
      ).toEqual(
        False
      );
    });
});