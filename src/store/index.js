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
    allInverviews: AaJson.default,
    currentLang: 'nl4eng',
    currentPrompt: 0,
  },
  getters: {
    allLangs(state) {
      return aaJsonLangs(state.allInverviews);
    },
    questions(state, getters) {
      const currentInterviewBody = Object.values(getters.currentInterview)[0];
      return AnyAll.paint2(state.marking)(currentInterviewBody);
    },
    currentInterview(state) {
      return state.allInverviews[state.currentLang][state.currentPrompt];
    },
    questionPrompt(state, getters) {
      return Object.keys(getters.currentInterview)[0];
    },
    getMarkingField: (state) => (id) => {
      return state.marking[id]
    },
    getTopLevelDecisionKeys(state) {
      return state.allInverviews[state.currentLang].flatMap((x) => Object.keys(x));
    }
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
