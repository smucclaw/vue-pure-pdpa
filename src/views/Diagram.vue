<template>
  <div class="columns">
    <section class="column">
      <Notification :theme-color='responseTheme'>
        <p class="title is-spaced">{{ questionPrompt }}</p>
        <p class="subtitle">{{ responseMsg }}</p>
      </Notification>
      <D3
        class="block"
        :qroot="questions"
      >
      </D3>
    </section>
  </div>
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Notification from '@/components/questions/Notification.vue';
import D3 from '@/components/viz/D3.vue';

export default {
  name: 'Home',
  components: {
    Notification,
    D3,
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
