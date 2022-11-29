<template>
  <!-- <div class="vertical-container">
    <p>aaa</p>
    <p>bbb</p>
    <p>ccc</p>
  </div> -->
  <TheQuestion v-slot="slotProps">
    <div class="columns">
      <div class="column is-one-fifth has-text-left is-size-4">
        <div class="vertical-container">Assessment</div>
        <div class="vertical-container">Notify Individuals Notify PDPC</div>
        <div class="vertical-container">The Third Thing</div>
      </div>
      <div class="column has-text-left is-size-6">
        <QuestionBase :question="slotProps.questions" />
      </div>
    </div>
  </TheQuestion>
  <!-- <div class="vertical-container">
    <p>aaa</p>
    <p>bbb</p>
    <p>ccc</p>
  </div> -->
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
