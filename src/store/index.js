import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import * as AnyAll from '../AnyAll.purs';
import * as InterviewConfig from '../RuleLib/Interview.purs';

import { BoolVar, AllQuantifier, AnyQuantifier } from "ladder-diagram"


function getLins(InterviewConfig) {
  const allLins = {}
  for (const e of InterviewConfig.allLang) {
    allLins[e] = (InterviewConfig[e])
  }
  return allLins
}

export default createStore({
  state: {
    marking: AnyAll.emptyMarking,
    interviewRules: InterviewConfig.interviewRules,
    interviewRules_nl: InterviewConfig.interviewRules_nl,
    topLD: InterviewConfig.nl4eng,
    topLDBody: '',
    // topS: InterviewConfig.toplevelStatements,
    whichPrompt: 0,
    objects: getLins(InterviewConfig),
    allLangs: InterviewConfig.allLang
  },
  getters: {
    getField,
    langs(state) {
      return state.allLangs;
    },
    questions(state) {
      if (!state.topLDBody) {
        const topLDBody = Object.values(state.topLD)[state.whichPrompt];
        return AnyAll.paint(AnyAll.hard)(state.marking)(state.interviewRules_nl)(topLDBody);
      }
      return AnyAll.paint(AnyAll.hard)(state.marking)(state.interviewRules_nl)(state.topLDBody);
    },
    questionPrompt(state) {
      const heads = AnyAll.heads(state.topLD);
      return heads;
    },
    // statements(state) {
    //   const s = AnyAll.paint(AnyAll.hard)(state.marking)(state.interviewRules_nl)(Object.values(state.topS)[state.whichPrompt]);
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
