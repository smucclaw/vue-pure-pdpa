<template>
  <div class="question-block" v-if="!question.andOr.contents">
    <div class="question-content" v-if="question.prePost.Pre" :class="theme">
      <div class="is-single-question" :style="indentParent" :data-test="question.prePost.Pre">
        <strong><em>{{ question.prePost.Pre }}</em> {{ question.andOr.nl.en }}</strong>
      </div>
    </div>
    <template v-if="question.andOr.children">
      <QuestionBase v-for="child in question.andOr.children" :key="child" :question="child"
        :parent-tag="question.andOr.tag" :parent-view="question.shouldView" :depth="newDepth" />
    </template>
  </div>
  <div class="question-content" v-if="question.andOr.contents" :class="theme">
    <div class="is-single-question" :style="indentParent">
      <div class="is-asking">
        <strong v-if="question.andOr.nl.en">
          <em>{{ question.andOr.contents }}.</em>
          {{ question.andOr.nl.en }}
        </strong>
        <strong v-else>{{ question.andOr.contents }}</strong>
      </div>
      <div class="is-answering" :data-test="question.andOr.contents">
        <QuestionRadio v-model="leaf" :disabled="isHidden" />
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import {interviewStore} from '@/store/index.js';
import {userMark} from '@/model/MarkDetails';
import QuestionRadio from "@/components/QuestionRadio.vue";
import { Ternary, ternaryFromString } from '@/model/Ternary';

const store = interviewStore()

const props = defineProps({
  question: Object,
  parentTag: String,
  parentView: String,
  depth: {
    type: Number,
    default: 0
  }
})

const isHidden = computed(() => {
  const isHideMode = props.question.shouldView === "Hide"
  const isViewMode = props.question.shouldView === "View"
  const isParentViewHide = props.parentView === "Hide"
  const isTagLeaf = props.question.andOr.tag === "Leaf"

  return isHideMode || (isTagLeaf && isViewMode && isParentViewHide)
})

const theme = computed(() =>
  isHidden.value
    ? "has-background-grey-lighter has-text-grey-light"
    : "has-background-light"
)

const newDepth = computed(() => props.depth + 1)

const indentParent = computed(() => indent(newDepth.value))

const leaf = computed({
  get: () => props.question.mark.source === "user" ? props.question.mark.value : "none",
  set: (value) => {
    console.log(value)
    console.log(    ternaryFromString(value)    )
    console.log(    userMark(value)    )

    store.updateMarkingField(
      props.question.andOr.contents,
      userMark(value),
    )
  }
})

function indent(depth, spacingFactor = 0.75) {
  return {
    marginLeft: `${depth * spacingFactor}rem`,
  }
}
</script>

<style scoped>
.question-block .question-content {
  margin-top: 0.25rem;
  margin-bottom: 0.25rem;
  padding: 0.5rem 0;
  border-radius: 4px;
  width: 60vw;
  display: flex;
  flex-wrap: wrap;
}

.question-block .question-content:last-child {
  margin-bottom: 0;
}

.question-block .question-content .is-single-question {
  margin-right: 0.5rem;
  margin-top: 0;
  margin-bottom: 0;
  gap: 0.5rem;
}

.question-block .question-content .is-asking {
  flex-grow: 1;
}

.question-block .question-content .is-answering {
  flex-shrink: 0;
  text-align: right;
}
</style>
