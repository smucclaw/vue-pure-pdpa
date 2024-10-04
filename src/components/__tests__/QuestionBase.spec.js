import { describe, it, expect } from "vitest";
import { mount } from "@vue/test-utils";

import QuestionBase from "../QuestionBase.vue";

const q = {
  andOr: { tag: "Leaf", nl: {}, contents: "does the person walk?" },
  mark: { value: "undefined", source: "user" },
  prePost: {},
  shouldView: "Ask",
};

const valueSelector = "[data-test='does the person walk?']";

describe("Test QuestionBase Component", () => {
  it("renders properly", () => {
    const wrapper = mount(QuestionBase, { props: { question: q } });
    expect(wrapper.find(valueSelector).text()).toContain('Yes')
  });
});
