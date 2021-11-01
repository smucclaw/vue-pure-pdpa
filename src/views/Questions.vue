<template lang='pug'>
.columns
  //-aside.column.is-one-third
    span.h3 Menu
    // Menu(:list-items='menuItems')
  section.column
    Notification(:theme-color='theme')
      .title.is-spaced Must you sing?
      .subtitle Yes
    .block.has-text-left-tablet
      Question(:question-list='questions')
</template>

<script>
import data from '@/utils/anyall.json';
import Menu from '@/components/questions/Menu.vue';
import Notification from '@/components/questions/Notification.vue';
import Question from '@/components/questions/Question.vue';

export default {
  name: 'Questions',
  data() {
    return {
      questions: data.andOr.children,
      theme: 'is-info',
    };
  },
  components: {
    Question,
    Menu,
    Notification,
  },
  mounted() {
  },
};
</script>
