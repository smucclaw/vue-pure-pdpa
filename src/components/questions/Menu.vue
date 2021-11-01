<template lang="pug">
.menu
  p.menu-label Questions
  ul.menu-list.has-text-left-tablet
    li(v-for='item in items')
      a {{ item }}
</template>

<script>
export default {
  name: 'Menu',
  props: {
    listItems: Array,
  },
  data() {
    return {
      items: this.listItems,
    };
  },
};
</script>
