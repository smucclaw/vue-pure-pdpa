<template>
  <TheMain>
    <D3
      class="block"
      :qroot="questions"
      />
  </TheMain>
</template>

<script>
import { mapGetters } from 'vuex';
import TheMain from '@/components/TheMain.vue';
import D3 from '@/components/viz/D3.vue';

export default {
  name: 'Diagram',
  components: {
    TheMain,
    D3,
  },
  computed: {
    ...mapGetters(['questions']),
  },
};
</script>
