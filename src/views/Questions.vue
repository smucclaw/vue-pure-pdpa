<template lang='pug'>
.section
  .columns
    aside.column.is-one-third
      .menu
        p.menu-label Question List
        ul.menu-list
          li(v-for='question in questions')
            a {{ question.title }}
    .column
      Question(
        v-for='question in questions',
        :title='question.title',
        :choices='question.options'
      )
</template>

<script>
import Question from '@/components/Question.vue';

export default {
  name: 'Questions',
  data() {
    return {
      questions: [
        {
          title: 'Are buttons allowed?',
          options: [
            'Yes',
            'No',
            'I don\'t know',
          ],
        },
        {
          title: 'Are buttons allowed again?',
          options: [
            'Yes',
            'No',
            'I don\'t know',
            'Lorem ipsum',
          ],
        },
      ],
    };
  },
  components: {
    Question,
  },
};
</script>
