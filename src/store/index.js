import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import * as AnyAll from '../AnyAll.purs';
import * as PDPA from '../RuleLib/PDPADBNO.purs';

export default createStore({
  state: {
    marking: AnyAll.emptyMarking,
    rulesPDPA: PDPA.schedule1_part1,
    rulesPDPA_nl: PDPA.schedule1_part1_nl,
    toplevelDecisions: PDPA.toplevelDecisions,
  },
  getters: {
    getField,
    questions(state) {
      return AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(state.rulesPDPA);
    },
    questionPrompt() {
      return 'Must you notify?';
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
  },
  actions: {
  },
  modules: {
  },
});
