<template lang="pug">
.card.card-border.is-size-6.has-text-left(
  :class='theme.background',
  v-if='!question.andOr.contents'
  )
  .card-content.p-4(v-if='question.prePost.pre')
    p.title.is-5(:class='theme.text') {{ question.prePost.pre }} {{ question.andOr.nl.en }}
  template(v-if='question.andOr.children')
    .card-content.p-4(v-for='child in question.andOr.children', :key='child')
      Question(
        :question='child',
        :parent-tag='question.andOr.tag',
        :parent-view='question.shouldView',
        :depth='newDepth',
        )
.columns(:class='[theme.background, theme.text]', v-if='question.andOr.contents')
  .column.is-flex-grow-2.has-text-left {{ question.andOr.contents }} {{ question.andOr.nl.en }}
  .column.has-text-right(v-if='!isHidden')
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
    depth: Number,
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
        ? {
          background: 'has-background-grey-lighter',
          text: 'has-text-grey-light',
        }
        : {
          background: '',
          text: '',
        };
    },
    newDepth() {
      return this.depth + 1;
    },
    indent() {
      return {
        margin: `0 0 0 ${this.depth * 0.5}em`,
      };
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
};
</script>
