import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import * as AnyAll from '../AnyAll.purs';
import * as PDPA from '../RuleLib/PDPADBNO.purs';

export default createStore({
  state: {
    marking: AnyAll.emptyMarking,
    rulesPDPA: PDPA.schedule1_part1,
    rulesPDPA_nl: PDPA.schedule1_part1_nl,
    tldPDPA: PDPA.toplevelDecisions,
    topLD: PDPA.toplevelDecisions,
    topLDBody: '',
    whichPrompt: 1,
  },
  getters: {
    getField,
    questions(state) {
      if (!state.topLDBody) {
        const topLDBody = Object.values(state.topLD)[state.whichPrompt];
        console.log(topLDBody);
        return AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(topLDBody);
      }
      return AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(state.topLDBody);
    },
    questionPrompt(state) {
      const heads = Object.keys(state.tldPDPA);
      console.log(heads);
      // return heads;
      return heads;
    },
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
  },
  actions: {
  },
  modules: {
  },
});
