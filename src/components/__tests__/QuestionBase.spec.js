import { describe, it, expect } from "vitest";
import { mount } from "@vue/test-utils";
import { createTestingPinia } from "@pinia/testing";
import { vi } from 'vitest'

import QuestionBase from "../QuestionBase.vue";

const q = {
  andOr: { tag: "Leaf", nl: {}, contents: "does the person walk?" },
  mark: { value: "undefined", source: "user" },
  prePost: {},
  shouldView: "Ask",
};

const qq = {
  andOr: {
    tag: "All",
    nl: {},
    children: [
      {
        andOr: { tag: "Leaf", nl: {}, contents: "does the person walk?" },
        mark: { value: "undefined", source: "user" },
        prePost: {},
        shouldView: "Ask",
      },
      {
        andOr: {
          tag: "Any",
          nl: {},
          children: [
            {
              andOr: { tag: "Leaf", nl: {}, contents: "does the person eat?" },
              mark: { value: "undefined", source: "user" },
              prePost: {},
              shouldView: "Ask",
            },
            {
              andOr: { tag: "Leaf", nl: {}, contents: "does the person drink?" },
              mark: { value: "undefined", source: "user" },
              prePost: {},
              shouldView: "Ask",
            },
          ],
        },
        mark: { value: "undefined", source: "user" },
        prePost: { Pre: "any of:" },
        shouldView: "View",
      },
    ],
  },
  mark: { value: "undefined", source: "user" },
  prePost: { Pre: "all of:" },
  shouldView: "View",
};

const valueSelectorChild = "[data-test='does the person walk?']";
const valueSelectorNode = "[data-test='all of:']";

describe("Test QuestionBase Component", () => {
  it("renders leaf properly", () => {
    const wrapper = mount(QuestionBase, {
      props: { question: q },
      global: {
        plugins: [createTestingPinia({
          createSpy: vi.fn,
        })],
      },
    });
    expect(wrapper.find(valueSelectorChild).text()).toContain("Yes");
  });

  it("renders tree node properly", () => {
    const wrapper = mount(QuestionBase, {
      props: { question: qq },
      global: {
        plugins: [createTestingPinia({
          createSpy: vi.fn,
        })],
      },
    });
    expect(wrapper.find(valueSelectorNode).text()).toContain("all of:");
  });
});
