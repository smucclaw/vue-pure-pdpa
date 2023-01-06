<template>
  <div class="question-block" v-if="!question.andOr.contents">
    <div class="question-content" v-if="question.prePost.pre" :class="theme">
      <div class="is-single-question" :style="indentParent">
        <strong><em>{{ question.prePost.pre }}</em> {{ question.andOr.nl.en }}</strong>
      </div>
    </div>
    <template v-if="question.andOr.children">
      <QuestionBase
        v-for="child in question.andOr.children"
        :key="child"
        :question="child"
        :parent-tag="question.andOr.tag"
        :parent-view="question.shouldView"
        :depth="newDepth"
      />
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
      <div class="is-answering" v-if="!isHidden">
        <QuestionRadio v-model="leaf" />
      </div>
    </div>
  </div>
</template>

<script>
import QuestionRadio from '@/components/QuestionRadio.vue';

export default {
  name: 'QuestionBase',
  props: {
    question: Object,
    parentTag: String,
    parentView: String,
    depth: {
      type: Number,
      default: 0,
    },
  },
  components: {
    QuestionRadio,
  },
  computed: {
    isHidden() {
      const isHideMode = this.question.shouldView === 'Hide';
      const isViewMode = this.question.shouldView === 'View';
      const isParentViewHide = this.parentView === 'Hide';
      const isTagLeaf = this.question.andOr.tag === 'Leaf';

      return isHideMode || (isTagLeaf && isViewMode && isParentViewHide);
    },
    theme() {
      return this.isHidden
        ? 'has-background-grey-lighter has-text-grey-light'
        : 'has-background-light';
    },
    newDepth() {
      return this.depth + 1;
    },
    indentParent() {
      return this.indent(this.newDepth);
    },
    leaf: {
      get() {
        return this.question.mark.source === 'user'
          ? this.question.mark.value : 'none';
      },
      set(value) {
        this.$store.commit('updateMarkingField', {
          question: this.question.andOr.contents,
          answer: {
            source: 'user',
            value,
          },
        });
      },
    },
  },
  methods: {
    indent(depth, spacingFactor = 0.75) {
      return {
        marginLeft: `${depth * spacingFactor}rem`,
      };
    },
  },
};
</script>

<style lang="scss" scoped>
.question-block{
  .question-content{
    margin-top: 0.25rem;
    margin-bottom: 0.25rem;
    padding: 0.5rem 0;
    border-radius: 4px;
    &:last-child{
      margin-bottom: 0;
    }
    .is-single-question{
      margin-right: 0.5rem;
      margin-top: 0;
      margin-bottom: 0;
      gap: 0.5rem;
    }
    .is-asking{
      flex-grow: 1;
    }
    .is-answering{
      flex-shrink: 0;
      text-align: right;
    }
  }
}
</style>
