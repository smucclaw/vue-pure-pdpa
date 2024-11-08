import { describe, it, expect } from "vitest";
import { mount } from "@vue/test-utils";
import { createStore } from 'vuex';

import TheQuestion from "../TheQuestion.vue";

const store = createStore({
  state: {
    marking: {},
    allInverviews: {'nl4eng':{}},
    currentLang: 'nl4eng',
    currentPrompt: 0,
  },
  getters: {
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

const $store = store;

describe("Test TheQuestion Component", () => {
  it("Question prompt should be Walking?", () => {
    const wrapper = mount(TheQuestion, {
      global: {
        provide: {
          store: store
        },
        mocks: {
          $store,
        },
      },
    });

    const questionPrompt = wrapper.get('[data-testid="question-prompt"]');
    expect(questionPrompt.text()).toBe("Walking?");
  });
});
