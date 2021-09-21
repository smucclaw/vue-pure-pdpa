<template>
  <form>
    <h1 class="title">You Must Sing If</h1>
    <Q v-bind:q='qroot' v-bind:depth=0 />
  </form>
  <HelloWorld v-bind:msg='qroot' />
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Q from '@/components/Q.vue';
import HelloWorld from '@/components/HelloWorld.vue';

export default {
  name: 'MustSing',
  props: {
  },
  computed: {
    ...mapFields(['marking', 'anyallform', 'formTitle']),
    ...mapGetters(['qroot']),
  },
  components: {
    Q,
    HelloWorld,
  },
};
</script>
