import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import { str } from '../index.purs';
import * as AnyAll from '../AnyAll.purs';
import * as PDPA from '../RuleLib/PDPADBNO.purs';

const isMustSing = false;

export default createStore({
  state: {
    pur_str: AnyAll.fromNode2(str),
    anyallform: AnyAll.anyallform1,
    marking: AnyAll.emptyMarking,
    rulesExample1: AnyAll.example1,
    nlExample1: AnyAll.example1_nl,
    rulesPDPA: PDPA.schedule1_part1,
    rulesPDPA_nl: PDPA.schedule1_part1_nl,
    editor_str: 'press eval to parse input according to calc.jison grammar',
  },
  getters: {
    getField,
    questions(state) {
      return isMustSing
        ? AnyAll.paint(AnyAll.hard)(state.marking)(state.nlExample1)(state.rulesExample1)
        : AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(state.rulesPDPA);
    },
    questionPrompt() {
      return isMustSing ? 'Must you sing?' : 'Must you notify?';
    },
    getMarkingField(state) {
      return getField(state.marking);
    },
  },
  mutations: {
    /* eslint no-param-reassign: ["error", { "props": false }] */
    reset_editor_str(s) { s.editor_str = ''; },
    append_editor_str(s, x) { s.editor_str += x; },
    updateField,
    updateMarkingField(state, payload) {
      state.marking[payload.mField] = payload.vValue;
    },
  },
  actions: {
  },
  modules: {
  },
});
