<template>
  <TheMain v-slot="slotProps">
    <QuestionDiagram
      class="block"
      :question="slotProps.questions"
      />
  </TheMain>
</template>

<script>
import TheMain from '@/components/TheMain.vue';
import QuestionDiagram from '@/components/QuestionDiagram.vue';

export default {
  name: 'Diagram',
  components: {
    TheMain,
    QuestionDiagram,
  },
};
</script>
