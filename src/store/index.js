import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import * as AnyAll from '../AnyAll.purs';
import * as PDPA from '../RuleLib/PDPADBNO.purs';

import { BoolVar, AllQuantifier, AnyQuantifier } from "ladder-diagram"


function getLins(PDPA) {
  const allLins = {}
  for (const e of PDPA.allLang) {
    allLins[e] = (PDPA[e])
  }
  return allLins
}

export default createStore({
  state: {
    marking: AnyAll.emptyMarking,
    rulesPDPA: PDPA.schedule1_part1,
    rulesPDPA_nl: PDPA.schedule1_part1_nl,
    topLD: PDPA.nl4eng,
    topLDBody: '',
    // topS: PDPA.toplevelStatements,
    whichPrompt: 0,
    objects: getLins(PDPA),
    allLangs: PDPA.allLang
  },
  getters: {
    getField,
    langs(state) {
      return state.allLangs;
    },
    questions(state) {
      if (!state.topLDBody) {
        const topLDBody = Object.values(state.topLD)[state.whichPrompt];
        return AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(topLDBody);
      }
      return AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(state.topLDBody);
    },
    questionPrompt(state) {
      const heads = AnyAll.heads(state.topLD);
      return heads;
    },
    // statements(state) {
    //   const s = AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(Object.values(state.topS)[state.whichPrompt]);
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
