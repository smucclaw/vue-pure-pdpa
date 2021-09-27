<template lang="pug">
label.is-check.is-fullwidth.is-justify-content-flex-start.mb-2(
  :for='key'
)
  input(type='checkbox', :value='key')
  font-awesome-icon.icon.checkmark(icon='check')
  span {{ value }}
</template>

<script>
export default {
  name: 'Checkbox',
  props: {
    key: String,
    value: String,
  },
  data() {
    return {
    };
  },
  methods: {
    toggle() {
    },
  },
};
</script>

<style lang="scss">
</style>
