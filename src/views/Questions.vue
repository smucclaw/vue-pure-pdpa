<template>
  <TheQuestion v-slot="slotProps">
    <QuestionBase :question="slotProps.questions" />
    <LadderDiagram
      :question="slotProps.questions"
    />
  </TheQuestion>
</template>

<script>

import TheQuestion from '@/components/TheQuestion.vue';
import QuestionBase from '@/components/QuestionBase.vue';
import LadderDiagram from '@/components/LadderDiagram.vue';

export default {
  name: 'Questions',
  components: {
    TheQuestion,
    QuestionBase,
    LadderDiagram
  },
};
</script>
