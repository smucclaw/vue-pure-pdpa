<template>
  <TheQuestion v-slot="slotProps">
    <QuestionBase :question="slotProps.questions" />
  </TheQuestion>
</template>

<script>
import TheQuestion from '@/components/TheQuestion.vue';
import QuestionBase from '@/components/QuestionBase.vue';

export default {
  name: 'Questions',
  components: {
    TheQuestion,
    QuestionBase,
  },
};
</script>
