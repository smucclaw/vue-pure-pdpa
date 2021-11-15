<template lang='pug'>
.content
  h2 Hello World!
</template>

<script>
import { mapFields } from 'vuex-map-fields';

export default {
  name: 'Home',
  computed: {
    ...mapFields(['pur_str']),
  },
  components: {
  },
};
</script>
