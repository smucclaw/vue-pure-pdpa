<template>
  <TheMain v-slot="slotProps">
    <Question
      class="block has-text-left"
      :question="slotProps.questions"
      :depth=0
      />
  </TheMain>
</template>

<script>
import TheMain from '@/components/TheMain.vue';
import Question from '@/components/questions/Question.vue';

export default {
  name: 'Questions',
  components: {
    TheMain,
    Question,
  },
};
</script>
