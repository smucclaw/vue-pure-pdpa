import { defineStore } from 'pinia'
import * as AnyAll from 'anyall';
import * as AaJson from '../assets/Interview.json';
import { MarkDetails } from '@/model/MarkDetails';
import { relevant } from '@/model/Relevance';
import { Ternary } from '@/model/Ternary';

export function getAaJsonLins(aaJson) {
  return aaJson
}

export function aaJsonLangs(aaJson) {
  return Object.keys(aaJson)
}

export const interviewStore = defineStore('interview', {
  state: () => ({
    marking: new Map(),
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
      const oldQ = AnyAll.paint2(Object.fromEntries(state.marking))(currentInterviewBody);
      //const newQ = relevant(state.marking, Ternary.Unknown, currentInterviewBody);
      
      console.log('oldQ', oldQ);
      //console.log('newQ', newQ);
      return AnyAll.paint2(Object.fromEntries(state.marking))(currentInterviewBody);
    },
    currentInterview(state) {
      return state.allInverviews[state.currentLang][state.currentPrompt];
    },
    questionPrompt() {
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
    updateMarkingField(question: string, answer: MarkDetails) {
      this.marking.set(question, answer);
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
