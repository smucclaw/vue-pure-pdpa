<template>
  <TheQuestion v-slot="slotProps">
    <div class="block has-text-left is-size-6">
      <QuestionBase :question="slotProps.questions" />
    </div>
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
