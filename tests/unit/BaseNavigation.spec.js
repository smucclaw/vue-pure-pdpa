import { shallowMount } from '@vue/test-utils';
import BaseNavigation from '@/components/BaseNavigation.vue';

const factory = (propsData) => shallowMount(
  BaseNavigation, {
    propsData: {
      ...propsData,
    },
  },
);

describe('Test BaseNavigation Component', () => {
  it('initialises with correct elements', () => {
    const wrapper = factory();
    expect(wrapper.find('nav').exists()).toBe(true);
    expect(wrapper.find('.container').exists()).toBe(true);
    expect(wrapper.find('.navbar-brand').exists()).toBe(true);
    expect(wrapper.find('.navbar-menu').exists()).toBe(true);
  });

  describe('Props work correctly', () => {
    it('shows the correct navbar classes', () => {
      const wrapper = factory();
      expect(wrapper.find('nav').classes()).toContain('navbar');

      /*
      const classIsFixedTop = ['is-dark', 'is-fixed-top'];
      wrapper.setProps({
        navClasses: classIsFixedTop.join(' '),
      });
      expect(wrapper.find('nav').classes()).toContain(classIsFixedTop);

      const classIsFixedBottom = ['is-dark', 'is-fixed-bottom'];
      wrapper.setProps({
        navClasses: classIsFixedBottom.join(' '),
      });
      expect(wrapper.find('nav').classes()).toContain(classIsFixedBottom);
      */
    });

    it('has fluid width class if fluidWidth prop is false', () => {
      const wrapper = factory();
      expect(wrapper.find('.container').classes('is-fluid')).toBe(false);
    });

    it('has fluid width class if fluidWidth prop is true', () => {
      const wrapper = factory({ fluidWidth: true });
      expect(wrapper.find('.container').classes('is-fluid')).toBe(true);
    });

    it('hides the navbar burger if extendedMenu prop is false', () => {
      const wrapper = factory();

      expect(wrapper.find('.navbar-brand').find('a').exists()).toBe(false);
      expect(wrapper.find('.navbar-brand').find('span').exists()).toBe(false);
    });

    it('shows the navbar burger if extendedMenu prop is true', () => {
      const wrapper = factory({ extendedMenu: true });

      expect(wrapper.find('.navbar-brand').find('a').exists()).toBe(true);
      expect(wrapper.find('.navbar-brand').find('span').exists()).toBe(true);
    });
  });

  describe('Slots work correctly', () => {
    it.todo('shows navbar-start section if the start slot is present');

    it.todo('shows navbar-end section if the end slot is present');
  });
});
