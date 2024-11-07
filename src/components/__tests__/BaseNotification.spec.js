import { describe, it, expect } from 'vitest'
import { mount } from "@vue/test-utils";

import BaseNotification from '../BaseNotification.vue';


describe('Test BaseNotification Component', () => {

  it('notification is not light', () => {
    const wrapper = mount(BaseNotification, { props: { themeColor: "is-success" } })
    expect(wrapper.get('.is-success')).not.toBeNull()
  });
});
