import { shallowMount } from '@vue/test-utils';
import BaseNavigation from '@/components/BaseNavigation.vue';

const factory = (propsData) => shallowMount(BaseNavigation, {
  ...propsData,
});

describe('BaseNavigation', () => {
  it('renders correctly', () => {
    expect(true).toBe(true);
  });

  it('test 2', () => {
    const wrapper = factory({
    });
  });
});
