import { defineStore } from 'pinia'
import * as AnyAll from 'anyall';
import * as AaJson from '../assets/Interview.json';

export function getAaJsonLins(aaJson) {
  return aaJson
}

export function aaJsonLangs(aaJson) {
  return Object.keys(aaJson)
}

export const interviewStore = defineStore('interview', {
  state: () => ({
    marking: {},
    allInverviews: AaJson.default,
    currentLang: 'nl4eng',
    currentPrompt: 0,
  }),
  getters: {
    allLangs(state) {
      return aaJsonLangs(state.allInverviews);
    },
    questions(state) {
      const currentInterviewBody = Object.values(this.currentInterview)[0];
      return AnyAll.paint2(state.marking)(currentInterviewBody);
    },
    currentInterview(state) {
      return state.allInverviews[state.currentLang][state.currentPrompt];
    },
    questionPrompt(state) {
      return Object.keys(this.currentInterview)[0];
    },
    getMarkingField: (state) => (id) => {
      return state.marking[id]
    },
    getTopLevelDecisionKeys(state) {
      return state.allInverviews[state.currentLang].flatMap((x) => Object.keys(x));
    }
  },
  actions: {
    updateMarkingField(payload) {
      this.marking[payload.question] = payload.answer;
    },
    updateCurrentPrompt(payload) {
      this.currentPrompt = payload;
    },
    updateLang(payload) {
      this.currentLang = payload;
    },
  },
  modules: {
  },
});
