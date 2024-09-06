import { evaluate } from '@ps/AnyAll.Relevance';
import { Leaf, Default, Marking } from '@ps/AnyAll.Types/index.js';
import { Just } from '@ps/Data.Maybe';
import { singleton } from '@ps/Data.Map';

const keyString = 'key';
const keyLeaf = new Leaf('key');

function makeRightMarking(mark : boolean) {
  const defaultR = Default(new Just(mark));

  return Marking(singleton(keyString)(defaultR));
}

describe('evaluate', () => {
    it('right key present in marking and True', () => {
      expect(
        evaluate(makeRightMarking(true))(keyLeaf)
      ).toEqual(
        new Just(true)
      );
    });

    it('right key present in marking and False', () => {
      expect(
        evaluate(makeRightMarking(false))(keyLeaf)
      ).toEqual(
        new Just(false)
      );
    });
});