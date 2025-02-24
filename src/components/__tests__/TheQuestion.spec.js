import { describe, it, expect } from "vitest";
import { mount } from "@vue/test-utils";
import { createTestingPinia } from "@pinia/testing";
import { defineStore } from 'pinia'
import { vi } from 'vitest'

import TheQuestion from "../TheQuestion.vue";

const interviewStore = defineStore('interview',{
  state: () => ({ n: 0 }),
  getters: {
    questions() {
      return {
        mark: { value: "undefined", source: "user" },
      };
    },
    questionPrompt() {
      return "Walking?";
    },
    getTopLevelDecisionKeys() {
      return "Person";
    },
  },
})

describe("Test TheQuestion Component", () => {
  it("Question prompt should be Walking?", () => {
    const pinia = createTestingPinia({
      createSpy: vi.fn,
    })
    interviewStore(pinia)
    const wrapper = mount(TheQuestion, {
      global: {
        plugins: [pinia],
      },
    });

    const questionPrompt = wrapper.get('[data-testid="question-prompt"]');
    expect(questionPrompt.text()).toBe("Walking?");
  });
});
