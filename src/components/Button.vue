<template lang="pug">
button.button.has-text-left-tablet.is-fullwidth.is-justify-content-flex-start(
  :class='toggleActive.button', @click='toggle'
)
  font-awesome-icon.icon(:icon='toggleActive.icon')
  span {{ text }}
</template>

<script>
export default {
  props: {
    text: String,
  },
  data() {
    return {
      isActive: false,
      iconProps: {
        active: ['fas', 'check-square'],
        inactive: ['far', 'square'],
      },
    };
  },
  methods: {
    toggle() {
      this.isActive = !this.isActive;
    },
  },
  computed: {
    toggleActive() {
      return {
        button: this.isActive ? 'is-primary' : '',
        icon: this.isActive ? this.iconProps.active : this.iconProps.inactive,
      };
    },
  },
};
</script>

<style lang="scss">
</style>
