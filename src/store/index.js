import { createStore } from 'vuex';
import { getField, updateField } from 'vuex-map-fields';
import { str } from '../index.purs';
import * as AnyAll from '../AnyAll.purs';

export default createStore({
  state: {
    pur_str: AnyAll.fromNode2(str),
    anyallform: AnyAll.anyallform1,
    marking: AnyAll.marking1_encoded,
    rules: AnyAll.example1,
    nl: AnyAll.example1_nl,
    formTitle: 'test form 1',
    editor_str: 'press eval to parse input according to calc.jison grammar',
  },
  getters: {
    getField,
    qroot(state) {
      console.log('getter qroot running');
      console.log(state.marking);
      const toreturn = AnyAll.paint(AnyAll.hard)(state.marking)(state.nl)(state.rules);
      console.log(toreturn);
      return toreturn;
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
