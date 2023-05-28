<template>
  <TheQuestion v-slot="slotProps">
    <LadderDiagram
      class="block"
      :question="slotProps.questions"
    />
  </TheQuestion>
</template>

<script>
import TheQuestion from '@/components/TheQuestion.vue';
import LadderDiagram from '@/components/LadderDiagram.vue';

export default {
  name: 'Ladder',
  components: {
    TheQuestion,
    QuestionDiagram,
  },
};
</script>
