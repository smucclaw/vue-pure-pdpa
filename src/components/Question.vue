<template lang="pug">
.box.has-text-left-tablet
  h1.title {{ title }}
  .buttons
    Button(text='lorem ipsum dolor sit amet')
</template>

<script>
import Button from '@/components/Button.vue';

export default {
  name: 'Question',
  props: {
    title: String,
  },
  components: {
    Button,
  },
};
</script>

<style lang="scss">
.question {
  justify-content: flex-start !important;
}
</style>
