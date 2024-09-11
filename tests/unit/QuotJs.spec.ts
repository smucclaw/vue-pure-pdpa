import { evaluate } from '@ps/AnyAll.Relevance';
import * as AA from '@ps/AnyAll.Types/index.js';
import { singleton as singletonMap, empty as emptyMap } from '@ps/Data.Map';
import { Nothing, Just } from '@ps/Data.Maybe';
import { singleton as singletonArray } from '@ps/Data.Array';


const keyString = 'key';


function mkAskResponse(q: string) {
  return {
    "shouldView": "Ask",
    "prePost": {},
    "mark": {
      "source": "user",
      "value": "undefined"
    },
    "andOr": {
      "tag": "Leaf",
      "nl": {},
      "contents": q
    }
  }
}

function mkAskRequest(q: string) {
  const tagNl = emptyMap
  const andOr = new AA.Simply(q)
  const prePost = Nothing.value
  return AA.mkQ(AA.Ask.value)(andOr)(tagNl)(prePost)(AA.Unknown.value)([])
}

function mkOrRequest(children: any) {
  const tagNl = emptyMap
  const andOr = AA.Or.value
  const prePost = new Just(new AA.Pre("any of:"));
  return AA.mkQ(AA.View.value)(andOr)(tagNl)(prePost)(AA.Unknown.value)(children)
}

function mkOrResponse(children: any) {
  return {
    "shouldView": "View",
    "prePost": {
      "pre": "any of:"
    },
    "mark": {
      "source": "user",
      "value": "undefined"
    },
    "andOr": {
      "tag": "Any",
      "nl": {},
      "children": children
    }
  }
}

describe('evaluate', () => {
  it('Asks does the person drink?', () => {
    expect(
      AA.qoutjs(mkAskRequest("does the person drink?"))
    ).toEqual(
      mkAskResponse("does the person drink?")
    );
  });

  it('Asks does the person eat?', () => {
    expect(
      AA.qoutjs(mkAskRequest("does the person eat?"))
    ).toEqual(
      mkAskResponse("does the person eat?")
    );
  });

  it('Asks does the person eat?', () => {
    expect(
      AA.qoutjs(mkOrRequest([
        mkAskRequest("does the person eat?"),
        mkAskRequest("does the person drink?")
      ]))
    ).toEqual(
      mkOrResponse([
        mkAskResponse("does the person eat?"),
        mkAskResponse("does the person drink?")
      ])
    );
  });
});