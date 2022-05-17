<template>
  <TheMain v-slot="slotProps">
    <div class="block has-text-left is-size-6">
      <QuestionBase :question="slotProps.questions" />
    </div>
  </TheMain>
</template>

<script>
import TheMain from '@/components/TheMain.vue';
import QuestionBase from '@/components/QuestionBase.vue';

export default {
  name: 'Questions',
  components: {
    TheMain,
    QuestionBase,
  },
};
</script>
