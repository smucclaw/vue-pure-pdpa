<template>
  <section>
    <Notification class="is-always-on-top" :theme-color='responseTheme'>
      <p class="title is-spaced">{{ questionPrompt }}</p>
      <p class="subtitle">{{ responseMsg }}</p>
    </Notification>
    <div class="columns">
      <div class="column is-one-fifth has-text-left is-size-4">
        <Notification class="is-next-from-top">
          <div v-for="heading in Object.keys(this.$store.state.toplevelDecisions)"
            :key="heading" class="vertical-container">{{ heading }}</div>
          <!-- <div class="vertical-container">Assessment</div>
          <div class="vertical-container">Notify Individuals Notify PDPC</div>
          <div class="vertical-container">The Third Thing</div> -->
        </Notification>
        <!-- <div class="vertical-container">Assessment</div>
        <div class="vertical-container">Notify Individuals Notify PDPC</div>
        <div class="vertical-container">The Third Thing</div> -->
      </div>
      <div class="column has-text-left is-size-6">
        <slot :questions="questions"></slot>
      </div>
    </div>
    <!-- <slot :questions="questions"></slot> -->
  </section>
  <!-- <div class="vertical-container">
    <p>aaa</p>
    <p>bbb</p>
    <p>ccc</p>
  </div> -->
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
.is-next-from-top {
  top: 15rem !important;
  position: sticky;
  z-index: 900;
}
</style>
