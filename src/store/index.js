import { createStore } from 'vuex';
import * as AnyAll from 'anyall';
import * as AaJson from '../assets/Interview.json';

export function getAaJsonLins(aaJson) {
  return aaJson
}

export function aaJsonLangs(aaJson) {
  return Object.keys(aaJson)
}

export default createStore({
  state: {
    marking: {},
    interview: AaJson.default,
    currentLang: 'nl4eng',
    currentPrompt: 0,
  },
  getters: {
    langs(state) {
      return aaJsonLangs(state.interview);
    },
    questions(state) {
      const topLevelDecisions = state.interview[state.currentLang][0]
      const topLDBody = Object.values(topLevelDecisions)[state.currentPrompt];
      return AnyAll.paint2(state.marking)(topLDBody);
    },
    questionPrompt(state) {
      return Object.keys(state.interview[state.currentLang][0]);
    },
    getMarkingField: (state) => (id) => {
      return state.marking[id]
    },
    getTopLevelDecisions(state) {
      return state.interview[state.currentLang][0];
    },
  },
  mutations: {
    updateMarkingField(state, payload) {
      state.marking[payload.question] = payload.answer;
    },
    updateCurrentPrompt(state, payload) {
      state.currentPrompt = payload;
    },
    updateLang(state, payload) {
      state.currentLang = payload;
    },
  },
  actions: {
  },
  modules: {
  },
});
