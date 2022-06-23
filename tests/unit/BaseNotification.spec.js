// import { shallowMount } from '@vue/test-utils';
import BaseNotification from '@/components/BaseNotification.vue';

/*
const factory = (propsData) => shallowMount(BaseNotification, {
  ...propsData,
});
*/

describe('Test BaseNotification Component', () => {
  /*
  it('renders notification', () => {
    const themeDanger = 'is-danger';
    const wrapper = factory({
      themeColor: themeDanger,
    });

    expect(wrapper.find('div.notification').classes()).toContain(themeDanger);
  });
  */

  it('notification is not light', () => {
    const isLightSetting = {
      isLight: false,
    };

    expect(BaseNotification.computed.isLightTheme.call(isLightSetting)).toBe('');
  });

  it('notification is light', () => {
    const isLightSetting = {
      isLight: true,
    };

    expect(BaseNotification.computed.isLightTheme.call(isLightSetting)).toBe('is-light');
  });
});
