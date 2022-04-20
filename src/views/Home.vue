<template>
  <section>
    <Notification class="is-always-on-top" :theme-color='responseTheme'>
      <p class="title is-spaced">{{ questionPrompt }}</p>
      <p class="subtitle">{{ responseMsg }}</p>
    </Notification>
    <Question
      class="block has-text-left"
      :question="questions"
      :depth=0
    >
    </Question>
  </section>
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Notification from '@/components/questions/Notification.vue';
import Question from '@/components/questions/Question.vue';

export default {
  name: 'Home',
  components: {
    Question,
    Notification,
  },
  computed: {
    ...mapFields(['marking']),
    ...mapGetters(['questions', 'questionPrompt']),
    responseMsg() {
      return ({
        true: 'Yes!',
        false: 'No!',
        undefined: 'It depends...',
      })[this.questions.mark.value];
    },
    responseTheme() {
      return ({
        true: 'is-success',
        false: 'is-danger',
        undefined: 'is-info',
      })[this.questions.mark.value];
    },
  },
};
</script>

<style>
.is-always-on-top {
  top: 4rem !important;
  position: sticky;
  z-index: 900;
}
</style>
