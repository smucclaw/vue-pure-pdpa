<template>
  <section>
    <Notification class="is-always-on-top" :theme-color='responseTheme'>
      <p class="title is-spaced">{{ questionPrompt }}</p>
      <p class="subtitle">{{ responseMsg }}</p>
    </Notification>
    <div class="columns">
      <div class="column is-one-fifth has-text-left is-size-4">
        <Notification class="is-next-from-top clearEdges">
          <div v-for="(heading, index) in Object.keys(getTopLevelDecisions)"
            :key="heading" class="vertical-container"
            @click="changeQuestionPrompt(index)">{{ heading }}</div>
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
// import { mapGetters } from 'vuex';
import Notification from '@/components/BaseNotification.vue';

export default {
  name: 'TheQuestion',
  components: {
    Notification,
  },
  data() {
    return {
      whichPrompt: 0,
      whichHeading: '',
    };
  },
  computed: {
    questions() {
      return this.$store.getters.questions;
    },
    questionPrompt() {
      return this.$store.getters.questionPrompt[this.whichPrompt];
    },
    // ...mapGetters(['questions', 'questionPrompt']),
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
    getTopLevelDecisions() {
      return this.$store.state.topLD;
    },
  },
  methods: {
    changeQuestionPrompt(index) {
      this.whichPrompt = index;
      this.$store.state.whichPrompt = index;
      console.log(this.whichPrompt);
      this.$store.commit('updateTopLDBody', this.whichPrompt);
      // if (heading === 'Notify Individuals / Notify PDPC') {
      //   this.whichPrompt = 0;
      // } else if (heading === 'Assessment') {
      //   this.whichPrompt = 1;
      // }
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
  /* border: 5px solid white; */
}
.clearEdges {
  padding: 0px;
  margin: 0px;
}
</style>
