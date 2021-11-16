import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import { str } from '../index.purs';
import * as AnyAll from '../AnyAll.purs';
import * as PDPA from '../RuleLib/PDPADBNO.purs';

export default createStore({
  state: {
    pur_str: AnyAll.fromNode2(str),
    anyallform: AnyAll.anyallform1,
    marking: AnyAll.emptyMarking,
    rulesExample1: AnyAll.example1,
    nlExample1: AnyAll.example1_nl,
    rulesPDPA: PDPA.schedule1_part1,
    rulesPDPA_nl: PDPA.schedule1_part1_nl,
    formTitle: 'test form 1',
    editor_str: 'press eval to parse input according to calc.jison grammar',
  },
  getters: {
    getField,
    qrootExample1(state) {
      //      console.log('getter qroot running');
      //      console.log(state.marking);
      /* eslint-disable max-len */
      const toreturn = AnyAll.paint(AnyAll.hard)(state.marking)(state.nlExample1)(state.rulesExample1);
      //      console.log(toreturn);
      return toreturn;
    },
    qrootPDPA(state) {
      return AnyAll.paint(AnyAll.hard)(state.marking)(state.rulesPDPA_nl)(state.rulesPDPA);
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
      console.log('receiving updateMarkingField');
      console.log(payload);
      state.marking[payload.mField] = payload.vValue;
      console.log('marking is now');
      console.log(state.marking);
    },
  },
  actions: {
  },
  modules: {
  },
});
