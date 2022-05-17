<template lang="pug">
//-.card.card-border(
  :class='theme.background',
  v-if='!question.andOr.contents'
  )
  .card-content.py-4.px-0(v-if='question.prePost.pre')
    p.title.is-5(:class='theme.text') {{ question.prePost.pre }} {{ question.andOr.nl.en }}
  template(v-if='question.andOr.children')
    .card-content.py-4.px-0(v-for='child in question.andOr.children', :key='child')
      Question(
        :question='child',
        :parent-tag='question.andOr.tag',
        :parent-view='question.shouldView',
        :depth='newDepth',
        )
.question-block(v-if='!question.andOr.contents')
  .question-content(v-if='question.prePost.pre', :class='theme')
    .is-single-question(:style='indentParent')
      strong {{ question.prePost.pre }} {{ question.andOr.nl.en }}
  template(v-if='question.andOr.children')
    Question(
      v-for='child in question.andOr.children',
      :key='child',
      :question='child',
      :parent-tag='question.andOr.tag',
      :parent-view='question.shouldView',
      :depth='newDepth',
      )
.question-content(v-if='question.andOr.contents', :class='theme')
  .columns.is-single-question(:style='indentParent')
    .is-asking
      strong {{ question.andOr.contents }} {{ question.andOr.nl.en }}
    .is-answering(v-if='!isHidden')
      QuestionRadio(v-model='leaf')
</template>

<script>
import QuestionRadio from '@/components/QuestionRadio.vue';

export default {
  name: 'Question',
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
    }
  }
}
</style>
