<template lang="pug">
.notification(:class='themeColor')
  button.delete(:class='isCloseButtonVisible')
  slot Hello World
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
