<template>
  <div class="notification" :class="themeColor">
    <button class="delete" :class="isCloseButtonVisible" />
    <slot>Content</slot>
  </div>
</template>

<script>
export default {
  name: 'Notification',
  props: {
    hasCloseButton: Boolean,
    themeColor: String,
    isLight: Boolean,
    content: String,
  },
  data() {
    return {
    };
  },
  computed: {
    isCloseButtonVisible() {
      return this.hasCloseButton ? '' : 'is-hidden';
    },
    isLightTheme() {
      return this.isLight ? 'is-light' : '';
    },
  },
};
</script>
