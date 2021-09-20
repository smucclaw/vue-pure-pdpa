<template>
  <div class="anyallform">
    <h1 class="title">AnyAllForm: {{ anyallform }}</h1>
  </div>
</template>

<script>
export default {
  name: 'AnyAllForm',
  props: {
    anyallform: String,
  },
};
</script>
