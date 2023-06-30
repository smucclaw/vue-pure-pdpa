import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import * as AnyAll from '../AnyAll.purs';
import * as Interview from '../RuleLib/Interview.purs';

import { BoolVar, AllQuantifier, AnyQuantifier } from "ladder-diagram"


function getLins(Interview) {
  const allLins = {}
  for (const e of Interview.allLang) {
    allLins[e] = (Interview[e])
  }
  return allLins
}

export default createStore({
  state: {
    marking: AnyAll.emptyMarking,
    rulesInterview: Interview.interviewRules,
    rulesInterview_nl: Interview.interviewRules_nl,
    topLD: Interview.nl4eng,
    topLDBody: '',
    // topS: Interview.toplevelStatements,
    whichPrompt: 0,
    objects: getLins(Interview),
    allLangs: Interview.allLang
  },
  getters: {
    getField,
    langs(state) {
      return state.allLangs;
    },
    questions(state) {
      console.log(`store: state.topLDbody=${state.topLDBody}`)
      if (!state.topLDBody) {
	console.log(`store: ! state.topLDbody`)
        const topLDBody = Object.values(state.topLD)[state.whichPrompt];
	console.log(`store: topLDbody =`)
	console.log(topLDBody)
	let painted = AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesInterview_nl)(topLDBody);
	console.log(`store: paint result`)
	console.log(painted)
        return painted
      }

      console.log(`store: true state.topLDbody`)
      let painted = AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesInterview_nl)(state.topLDBody);
      console.log(`store: paint result`)
      console.log(painted)
      return painted
    },
    questionPrompt(state) {
      const heads = AnyAll.heads(state.topLD);
      return heads;
    },
    // statements(state) {
    //   const s = AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesInterview_nl)(Object.values(state.topS)[state.whichPrompt]);
    //   return s;
    // },
    getMarkingField(state) {
      return getField(state.marking);
    },


  },
  mutations: {
    updateField,
    updateMarkingField(state, payload) {
      state.marking[payload.question] = payload.answer;
    },
    updateTopLDBody(state, payload) {
      state.topLDBody = Object.values(state.topLD)[payload];
    },
    updateStatements(state, payload) {
    },
    updateLang(state, payload) {
      state.topLD = state.objects[payload];
    },
  },
  actions: {
  },
  modules: {
  },
});
