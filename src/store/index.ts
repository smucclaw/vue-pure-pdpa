import { defineStore } from 'pinia'
import * as AaJson from '../assets/Interview.json';
import { relevant } from '@/model/Relevance';
import { Ternary } from '@/model/Ternary';
import { deserializeItem } from '@/model/Item';
import { encodeJsonQ, InterviewViewModel } from '@/model/Interview';

export function getAaJsonLins(aaJson) {
  return aaJson
}

export function aaJsonLangs(aaJson) {
  return Object.keys(aaJson)
}

export const interviewStore = defineStore('interview', {
  state: () => ({
    marking: new Map<string, Ternary>(),
    allInverviews: AaJson.default,
    currentLang: 'nl4eng',
    currentPrompt: 0,
  }),
  getters: {
    allLangs(state) {
      return aaJsonLangs(state.allInverviews);
    },
    currentInterview(state) {
      return state.allInverviews[state.currentLang][state.currentPrompt];
    },
    questions(state): InterviewViewModel {
      const currentInterviewBody = Object.values(this.currentInterview)[0];
      const currentItem = deserializeItem(currentInterviewBody);
      const relevent = relevant(state.marking, Ternary.Unknown, currentItem);

      return encodeJsonQ(relevent);
    },
    questionPrompt() {
      return Object.keys(this.currentInterview)[0];
    },
    getMarkingField: (state) => {
      return (id: string) => state.marking.get(id)
    },
    getTopLevelDecisionKeys(state) {
      return state.allInverviews[state.currentLang].flatMap((x) => Object.keys(x));
    }
  },
  actions: {
    updateMarkingField(question: string, answer: Ternary) {
      this.marking.set(question, answer);
    },
    updateCurrentPrompt(payload: number) {
      this.currentPrompt = payload;
    },
    updateLang(payload: string) {
      this.currentLang = payload;
    },
  },
  modules: {
  },
});
