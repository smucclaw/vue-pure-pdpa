import { evaluate } from '@ps/AnyAll.Relevance';
import { Hard, Leaf, Default, Marking } from '@ps/AnyAll.Types/index.js';
import { Right } from '@ps/Data.Either';
import { Just } from '@ps/Data.Maybe';
import { singleton } from '@ps/Data.Map';

describe('Test TheMain Component', () => {
    it('expects to be true', () => {
      const jTrue = new Just(true);
      const right = new Right(jTrue);
      const defaultR = Default(right);

      const keyLeaf = new Leaf('key');
      const trueRightMarking = Marking(singleton("key")(defaultR));

      const mumboJumbo = evaluate(Hard.value)(trueRightMarking)(keyLeaf);
      expect(mumboJumbo).toEqual(jTrue);
    });
});