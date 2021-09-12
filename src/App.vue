<template>
  <div id="app">
    <h1> hello world! </h1>
  </div>
</template>

<script>
  const PMain = require('./PMain.purs');
  console.log("hello world");
  console.log(PMain.str);
</script>

<style>
body {
  margin: 0;
  padding: 0;
}
</style>