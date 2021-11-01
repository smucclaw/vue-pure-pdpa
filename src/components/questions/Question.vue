<template lang="pug">
.newquestion.is-danger
  .header All of
  .body
    .question(v-for='question in leafQuestions')
      .block {{ question.andOr.contents }}
      .control
        Radio
    .newquestion.is-warning.inner
      .header Either
      .body
        .question
          .block One
          .control
            Radio
</template>

<script>
import Radio from '@/components/questions/Radio.vue';

export default {
  props: {
    questionList: Object,
  },
  components: {
    Radio,
  },
  data() {
    return {
      amt: null,
      questions: this.questionList,
    };
  },
  computed: {
    leafQuestions() {
      return this.questions.filter((question) => question.andOr.tag === 'Leaf');
    },
  },
};
</script>

<style lang="scss">
</style>
