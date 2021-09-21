<template lang="pug">
.box.has-text-left-tablet
  h1.title Are buttons allowed?
  .buttons
    button.button.has-text-left-tablet.is-fullwidth.question
      font-awesome-icon(icon="check-square")
      |
      span Lorem ipsum dolor sit amet
    button.button.has-text-left-tablet.is-fullwidth.question
      font-awesome-icon(icon='check-square')
      span Lorem ipsum dolor sit amet
</template>

<style lang="scss">
.question {
  justify-content: flex-start !important;
}
</style>
