<template>
  <pre align='left'>{{ msg }}</pre>
</template>

<script>
export default {
  name: 'HelloWorld',
  props: {
    msg: Object,
  },
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped lang="scss">
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
