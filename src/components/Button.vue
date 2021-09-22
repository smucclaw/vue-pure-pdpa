<template lang="pug">
button.button.is-fullwidth.is-justify-content-flex-start(
  :class='toggleActive.button,toggleHidden', @click='toggle'
)
  font-awesome-icon.icon(:icon='toggleActive.icon')
  span {{ text }}
</template>

<script>
export default {
  props: {
    text: String,
    hidden: Boolean,
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
      this.$emit('selected', this.text);
    },
  },
  computed: {
    toggleActive() {
      return {
        button: this.isActive ? 'is-primary' : '',
        icon: this.isActive ? this.iconProps.active : this.iconProps.inactive,
      };
    },
    toggleHidden() {
      return this.hidden ? 'is-hidden' : '';
    },
  },
};
</script>

<style lang="scss">
</style>
