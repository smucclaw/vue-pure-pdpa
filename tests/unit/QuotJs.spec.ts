import { evaluate } from '@ps/AnyAll.Relevance';
import * as AA from '@ps/AnyAll.Types/index.js';
import * as AD from '@ps/Data.Argonaut.Decode.Class/index.js';
import { encodeJsonMarking, decodeMarkingArgo } from '@ps/AnyAll.Marking/index.js';
import { singleton as singletonMap, empty as emptyMap } from '@ps/Data.Map';
import { Nothing, Just } from '@ps/Data.Maybe';
import { singleton as singletonArray } from '@ps/Data.Array';

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
  return AA.mkQ(AA.Ask.value)(andOr)(prePost)(AA.Unknown.value)([])
}

function mkOrRequest(children: any) {
  const andOr = AA.Or.value
  const prePost = new Just(new AA.Pre("any of:"));
  return AA.mkQ(AA.View.value)(andOr)(prePost)(AA.Unknown.value)(children)
}

function mkAndRequest(children: any) {
  const andOr = AA.And.value
  const prePost = new Just(new AA.Pre("all of:"));
  return AA.mkQ(AA.View.value)(andOr)(prePost)(AA.Unknown.value)(children)
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

function mkAndResponse(children: any) {
  return {
    "shouldView": "View",
    "prePost": {
      "pre": "all of:"
    },
    "mark": {
      "source": "user",
      "value": "undefined"
    },
    "andOr": {
      "tag": "All",
      "nl": {},
      "children": children
    }
  }
}

describe('qoutjs', () => {
  it('Asks does the person drink?', () => {
    expect(
      AA.encodeJsonQ.encodeJson(mkAskRequest("does the person drink?"))
    ).toEqual(
      mkAskResponse("does the person drink?")
    );
  });

  it('Asks does the person eat?', () => {
    expect(
      AA.encodeJsonQ.encodeJson(mkAskRequest("does the person eat?"))
    ).toEqual(
      mkAskResponse("does the person eat?")
    );
  });

  it('Asks does the person eat?', () => {
    expect(
      AA.encodeJsonQ.encodeJson(mkOrRequest([
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

  it('Asks does the person eat?', () => {
    expect(
      AA.encodeJsonQ.encodeJson(mkAndRequest([
        mkAskRequest("does the person eat?"),
        mkAskRequest("does the person drink?")
      ]))
    ).toEqual(
      mkAndResponse([
        mkAskResponse("does the person eat?"),
        mkAskResponse("does the person drink?")
      ])
    );
  });
});

function marked(mark: string) {
  return {
    "source": "user",
    "value": mark
  }
}

describe('Ternary dumpDefault', () => {
  it('True', () => {
    expect(
      AA.dumpDefault(AA.True.value)
    ).toEqual(
      marked("true")
    );
  });

  it('False', () => {
    expect(
      AA.dumpDefault(AA.False.value)
    ).toEqual(
      marked("false")
    );
  });

  it('Unknown', () => {
    expect(
      AA.dumpDefault(AA.Unknown.value)
    ).toEqual(
      marked("undefined")
    );
  });
})

describe('decodeMarking', () => {
  it('Asks does the person drink?', () => {
    expect(
      encodeJsonMarking.encodeJson(decodeMarkingArgo({
        "a household appliance?": {
          "source": "user",
          "value": "true"
        },
        "birds?": {
          "source": "user",
          "value": "false"
        },
        "is Loss or Damage caused by birds?": {
          "source": "user",
          "value": "undefined"
        }
      }))
    ).toEqual(
      {
        "a household appliance?": {
          "source": "user",
          "value": "true"
        },
        "birds?": {
          "source": "user",
          "value": "false"
        },
        "is Loss or Damage caused by birds?": {
          "source": "user",
          "value": "undefined"
        }
      }
    );
  })
})