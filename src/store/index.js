import { createStore } from 'vuex';
import * as AnyAll from '../../anyall-purs/index.js';
import * as AaJson from '../assets/Interview.json';

function getLins(Interview) {
  const allLins = {}
  for (const e of Interview.allLang) {
    allLins[e] = (Interview[e])
  }
  return allLins
}

export function getAaJsonLins(aaJson) {
  return aaJson
}

export function aaJsonLangs(aaJson) {
  return Object.keys(aaJson)
}

export default createStore({
  state: {
    marking: AnyAll.emptyMarking,
    rulesInterview_nl: AnyAll.interviewRules_nl,
    topLD: AaJson.default["nl4eng"][0],
    marking: {},
    topLD: AaJson.default["nl4eng"][0],
    topLDBody: '',
    whichPrompt: 0,
    objects: getAaJsonLins(AaJson.default),
    allLangs: aaJsonLangs(AaJson.default)
  },
  getters: {
    langs(state) {
      return state.allLangs;
    },
    questions(state) {
      if (!state.topLDBody) {
        const topLDBody = Object.values(state.topLD)[state.whichPrompt];
        return AnyAll.paint2(state.marking)(topLDBody);
      }
      return AnyAll.paint2(state.marking)(state.topLDBody);
    },
    questionPrompt(state) {
      return Object.keys(state.topLD);
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
      console.log('payload', payload)
      state.topLD = state.objects[payload][0];
    },
  },
  actions: {
  },
  modules: {
  },
});
