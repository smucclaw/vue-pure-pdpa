<template lang='pug'>
.section
  .columns
    aside.column.is-one-third
      .menu
        p.menu-label Question List
        ul.menu-list
          li
            a.is-active Are checkboxes allowed?
          li
            a Are checkboxes allowed again?
    .column
      Question(title='Are buttons allowed?')
</template>

<script>
import Question from '@/components/Question.vue';

export default {
  name: 'Questions',
  components: {
    Question,
  },
};
</script>
