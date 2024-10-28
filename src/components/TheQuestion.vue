<template>
  <section>
    <Notification class="is-always-on-top" :theme-color="responseTheme">
      <p class="title is-spaced">{{ questionPrompt }}</p>
      <p class="subtitle" data-test="response-message">{{ responseMsg }}</p>
    </Notification>

    <div class="columns">
      <div class="column is-one-fifth has-text-left is-size-4">
        <Notification class="is-next-from-top clearEdges">
          <div
            v-for="(heading, index) in getTopLevelDecisions"
            :key="heading"
            class="vertical-container"
            @click="changeQuestionPrompt(index)"
          >
            {{ heading }}
          </div>
        </Notification>
      </div>
      <div class="column has-text-left is-size-6">
        <slot :questions="questions"></slot>
      </div>
    </div>
  </section>
</template>

<script>
import Notification from "@/components/BaseNotification.vue";

export default {
  name: "TheQuestion",
  components: {
    Notification,
  },
  data() {
    return {
      whichHeading: "",
    };
  },
  computed: {
    questions() {
      // console.log("questions", this.$store.getters.questions);
      return this.$store.getters.questions;
    },
    statements() {
      // console.log("statements", this.$store.getters.statements);
      return this.$store.getters.statements;
    },
    questionPrompt() {
      return this.$store.getters.questionPrompt;
    },
    responseMsg() {
      return {
        true: "Yes!",
        false: "No!",
        undefined: "It depends...",
      }[this.questions.mark.value];
    },
    responseTheme() {
      return {
        true: "is-success",
        false: "is-danger",
        undefined: "is-info",
      }[this.questions.mark.value];
    },
    getTopLevelDecisions() {
      return this.$store.getters.getTopLevelDecisionKeys;
    },
  },
  methods: {
    changeQuestionPrompt(index) {
      this.$store.commit("updateCurrentPrompt", index);
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
/* Max: added clearEdges */
.clearEdges {
  padding: 0px;
  margin: 0px;
}
</style>
