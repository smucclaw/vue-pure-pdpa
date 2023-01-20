<template>
  <section>
    <Notification class="is-always-on-top" :theme-color='responseTheme'>
      <p class="title is-spaced">{{ questionPrompt }}</p>
      <p class="subtitle">{{ responseMsg }}</p>
      {{statements}}
    </Notification>
    <slot :questions="questions" />
  </section>
</template>

<script>
import { mapGetters } from 'vuex';
import Notification from '@/components/BaseNotification.vue';

export default {
  name: 'TheQuestion',
  components: {
    Notification,
  },
  computed: {
    ...mapGetters(['questions', 'questionPrompt', 'statements']),
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
