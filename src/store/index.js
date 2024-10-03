import { createStore } from 'vuex';
import * as AnyAll from '../../anyall-purs/index.js';

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
    rulesInterview_nl: AnyAll.interviewRules_nl,
    topLD: AnyAll.nl4eng,
    topLDBody: '',
    whichPrompt: 0,
    objects: getLins(AnyAll),
    allLangs: AnyAll.allLang
  },
  getters: {
    langs(state) {
      return state.allLangs;
    },
    questions(state) {
      if (!state.topLDBody) {
        const topLDBody = Object.values(state.topLD)[state.whichPrompt];
        return AnyAll.paint(state.marking)(state.rulesInterview_nl)(topLDBody);
      }
      return AnyAll.paint(state.marking)(state.rulesInterview_nl)(state.topLDBody);
    },
    questionPrompt(state) {
      const heads = AnyAll.heads(state.topLD);
      return heads;
    },
    getMarkingField: (state) => (id) => {
      return state.marking[id]
    },
  },
  mutations: {
    updateMarkingField(state, payload) {
      state.marking[payload.question] = payload.answer;
    },
    updateTopLDBody(state, payload) {
      state.topLDBody = Object.values(state.topLD)[payload];
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
