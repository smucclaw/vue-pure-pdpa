<template lang="pug">
.control
  label.is-check.is-fullwidth.is-justify-content-flex-start
    input(type='checkbox', v-model='checked')
    font-awesome-icon.icon.checkmark(icon='check')
    span {{ text }}
</template>

<script>
export default {
  name: 'Checkbox',
  props: {
    text: String,
  },
  data() {
    return {
    };
  },
};
</script>

<style lang="scss">
</style>
