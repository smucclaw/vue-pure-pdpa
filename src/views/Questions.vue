<template lang='pug'>
.columns
  aside.column.is-one-third
    Notification(:theme-color='responseTheme')
      .title.is-spaced {{ questionPrompt }}
      .subtitle {{ responseMsg }}
    //-.block
      D3(:qroot='questions')
  section.column
    .block.has-text-left
      Question(:question='questions', :depth=0)
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Notification from '@/components/questions/Notification.vue';
import Question from '@/components/questions/Question.vue';
// import D3 from '@/components/D3.vue';

export default {
  name: 'Questions',
  components: {
    Question,
    Notification,
    // D3,
  },
  data() {
    return {
    };
  },
  methods: {
  },
  mounted() {
  },
  computed: {
    ...mapFields(['marking', 'anyallform', 'formTitle']),
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
