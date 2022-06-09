<template>
  <TheQuestion v-slot="slotProps">
    <QuestionDiagram
      class="block"
      :question="slotProps.questions"
      />
  </TheQuestion>
</template>

<script>
import TheQuestion from '@/components/TheQuestion.vue';
import QuestionDiagram from '@/components/QuestionDiagram.vue';

export default {
  name: 'Diagram',
  components: {
    TheQuestion,
    QuestionDiagram,
  },
};
</script>
