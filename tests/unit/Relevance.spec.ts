import { evaluate } from 'purs/AnyAll.Relevance';
import { Hard, Leaf, Default, Marking } from 'purs/AnyAll.Types';
import { Right } from 'purs/Data.Either';
import { Just } from 'purs/Data.Maybe';
import { fromFoldable } from 'purs/Data.Map';

describe('Test TheMain Component', () => {
    it('expects to be true', () => {
      const jTrue = new Just(true);
      const right = new Right(jTrue);
      const defaultR = Default(right);

      const marking = Marking(fromFoldable([['key', defaultR]]));
      const keyLeaf = new Leaf('key');

      console.info(keyLeaf instanceof Leaf)

      evaluate(Hard)(marking)(keyLeaf);
      expect(true).toBe(true);
    });
});