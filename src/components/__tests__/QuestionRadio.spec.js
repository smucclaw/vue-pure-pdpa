import { describe, it, expect } from 'vitest'
import { mount } from "@vue/test-utils";

import QuestionRadio from "../QuestionRadio.vue";

const valueSelector = "[data-test='does the person walk?']";

describe('Test QuestionRadio Component', () => {
  it('expects to be true', () => {
    const wrapper = mount(QuestionRadio, { props: { disabled: true } })
    expect(wrapper.findAll('input')).toHaveLength(3)
  });
});
