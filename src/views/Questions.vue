<template>
  <TheMain v-slot="slotProps">
    <div class="block has-text-left is-size-6">
      <Question
        :question="slotProps.questions"
        :depth=0
        />
    </div>
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
