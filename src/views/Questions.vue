<template>
  <TheMain>
    <Question
      class="block has-text-left"
      :question="questions"
      :depth=0
      />
  </TheMain>
</template>

<script>
import { mapGetters } from 'vuex';
import TheMain from '@/components/TheMain.vue';
import Question from '@/components/questions/Question.vue';

export default {
  name: 'Questions',
  components: {
    TheMain,
    Question,
  },
  computed: {
    ...mapGetters(['questions']),
  },
};
</script>
