<template lang='pug'>
div(class="column is-flex is-flex-direction-column")
  div(id="nav")
    router-link(to="/") Home
    span(class='space') |
    router-link(to="/about") About
    span(class='space') |
    router-link(to="/editor") Editor
    span(class='space') |
    router-link(to="/mustsing") Must Sing
    span(class='space') |
    router-link(to="/pdpa") PDPA
  router-view
</template>

<style lang="scss">
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  min-height: 100vh;
}

#nav {
  padding: 30px;

  a {
    font-weight: bold;
    color: #2c3e50;

    &.router-link-exact-active {
      color: #42b983;
    }
  }
}

.space {
  padding-left: 1em;
  padding-right: 1em;
}

.column {
  min-height: 100vh;
}

.viz-container {
  display: flex;
  flex-flow: row wrap;
  justify-content: space-around;
  padding: 0;
  margin: 0;
  list-style: none;
}

.viz-item {
  padding: 5px;
  width: 100vw;
  margin-top: 10px;
}
</style>
