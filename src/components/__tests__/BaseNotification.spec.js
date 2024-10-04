import { describe, it, expect } from 'vitest'
import BaseNotification from '@/components/BaseNotification.vue';


describe('Test BaseNotification Component', () => {

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
