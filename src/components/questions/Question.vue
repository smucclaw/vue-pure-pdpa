<template lang="pug">
.card.card-border.is-size-6(:class='theme.background')
  .card-header.mb-2(v-if='question.prePost.pre')
    .card-header-title(:class='theme.text')
      | {{ question.prePost.pre }}: {{ question.andOr.nl.en }}
  .card-content.p-4(v-if='question.andOr.contents')
    .card-header-title.p-0(:class='theme.text')
      | {{ question.andOr.contents }}: {{ question.andOr.nl.en }}
    .control.mt-2(v-if='!isHidden')
      QuestionRadio(v-model='leaf')
  .card-content.px-2.pt-0.pb-2(
    v-if='question.andOr.children',
    v-for='child in question.andOr.children',
    :key='child',
  )
    Question(
      :question='child',
      :parent-tag='question.andOr.tag',
      :parent-view='question.shouldView',
      :depth='newDepth',
    )
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
  data() {
    return {
      btnOptions: [
        {
          name: 'Yes',
          value: 'true',
        },
        {
          name: 'Don\'t Know',
          value: 'undefined',
        },
        {
          name: 'No',
          value: 'false',
        },
      ],
    };
  },
  methods: {
    isTag(tag) {
      const isCurrentTag = this.question.andOr.tag === tag;
      const isParentTag = this.parentTag === tag;
      const isViewMode = this.question.shouldView === 'View';
      const isUserSource = this.question.mark.source === 'user';
      const isTrueValue = this.question.mark.value === 'true';

      return isCurrentTag || (isViewMode && isParentTag && isUserSource && isTrueValue);
    },
    isDefaultValue(value) {
      return this.question.mark.source === 'default'
        && this.question.mark.value === value;
    },
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
      // return this.isTag('All') ? 'is-danger' : 'is-warning';
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
          mField: this.question.andOr.contents,
          vValue: {
            source: 'user',
            value,
          },
        });
      },
    },
  },
};
</script>

<style lang="scss">
</style>
