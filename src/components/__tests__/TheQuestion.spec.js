import { describe, it, expect } from "vitest";
import { mount } from "@vue/test-utils";
import { createStore } from "vuex";

import TheQuestion from "../TheQuestion.vue";

const store = createStore({
  getters: {
    questions(state, getters) {
      return {
        mark: { value: "undefined", source: "user" },
      };
    },
    questionPrompt(state, getters) {
      return "Walking?";
    },
    getTopLevelDecisionKeys(state) {
      return "Person";
    },
  },
});

describe("Test TheQuestion Component", () => {
  it("Question prompt should be Walking?", () => {
    const wrapper = mount(TheQuestion, {
      global: {
        provide: {
          store: store,
        },
      },
    });

    const questionPrompt = wrapper.get('[data-testid="question-prompt"]');
    expect(questionPrompt.text()).toBe("Walking?");
  });
});
