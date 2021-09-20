<template>
  <p>AnyAllForm top level</p>
  <div class="anyallform">
    <h1 class="title">AnyAllForm: {{ formTitle }}</h1>
    <Q v-bind:q='anyallform' v-bind:depth=0 />
  </div>
</template>

<script>
import Q from '@/components/Q.vue';

export default {
  name: 'AnyAllForm',
  props: {
    anyallform: Object,
    formTitle: String,
  },
  components: { Q },
};
</script>
