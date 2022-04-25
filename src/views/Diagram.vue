<template>
  <TheMain>
    <QuestionDiagram
      class="block"
      :question="questions"
      />
  </TheMain>
</template>

<script>
import { mapGetters } from 'vuex';
import TheMain from '@/components/TheMain.vue';
import QuestionDiagram from '@/components/QuestionDiagram.vue';

export default {
  name: 'Diagram',
  components: {
    TheMain,
    QuestionDiagram,
  },
  computed: {
    ...mapGetters(['questions']),
  },
};
</script>
