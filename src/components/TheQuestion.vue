<template>
  <section>
    <Notification class="is-always-on-top" :theme-color="responseTheme">
      <p class="title is-spaced" data-testid="question-prompt">{{ questionPrompt }}</p>
      <p class="subtitle" data-test="response-message">{{ responseMsg }}</p>
    </Notification>

    <div class="columns">
      <div class="column is-one-fifth has-text-left is-size-4">
        <Notification class="is-next-from-top clearEdges">
          <div v-for="(heading, index) in getTopLevelDecisions" :key="heading" class="vertical-container"
            @click="changeQuestionPrompt(index)">
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

<script setup>
import { computed } from "vue";
import { useStore } from 'vuex';

import Notification from "@/components/BaseNotification.vue";

const store = useStore();

const questions = computed(() => {
  return store.getters.questions;
})

const questionPrompt = computed(() => {
  return store.getters.questionPrompt;
});

const responseMsg = computed(() => {
  return {
    true: "Yes!",
    false: "No!",
    undefined: "It depends...",
  }[questions.value.mark.value]
});

const responseTheme = computed(() => {
  return {
    true: "is-success",
    false: "is-danger",
    undefined: "is-info",
  }[questions.value.mark.value]
});

const getTopLevelDecisions = computed(() => {
  return store.getters.getTopLevelDecisionKeys;
});

function changeQuestionPrompt(index) {
  store.commit("updateCurrentPrompt", index);
}
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

.clearEdges {
  padding: 0px;
  margin: 0px;
}
</style>
