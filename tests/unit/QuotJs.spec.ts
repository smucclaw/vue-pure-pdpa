import { evaluate } from '@ps/AnyAll.Relevance';
import { Leaf, View, qoutjs, Q, Simply, Unknown, mkEasyQ } from '@ps/AnyAll.Types/index.js';
import { singleton as singletonMap } from '@ps/Data.Map';
import { Nothing } from '@ps/Data.Maybe';
import { singleton as singletonArray } from '@ps/Data.Array';


const keyString = 'key';

describe('evaluate', () => {
  it('right key present in marking and True', () => {
    const tagNl = singletonMap(keyString)("mark")
    const andOr = new Simply("key")
    const prePost = Nothing.value
    expect(
      qoutjs(
        mkEasyQ(View.value)(andOr)(tagNl)(prePost)(Unknown.value)
      )
    ).toEqual(
      { "andOr": { "contents": "key", "nl": { "key": "mark" }, "tag": "Leaf" }, "mark": { "source": "user", "value": "undefined" }, "prePost": {}, "shouldView": "View" }
    );
  });

});