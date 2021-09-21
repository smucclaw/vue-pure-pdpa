<template lang="pug">
nav.navbar.is-dark
  .container
    .navbar-brand
      router-link.navbar-item(to='/') Dolores
      a.navbar-burger(
        role='button',
        aria-label='menu',
        aria-expanded='false'
      )
        span(aria-hidden='true')
        span(aria-hidden='true')
        span(aria-hidden='true')
    .navbar-menu
      .navbar-end
        router-link.navbar-item(to='/about') About
        router-link.navbar-item(to='/questions') Questions
        router-link.navbar-item(to='/editor') Editor
</template>
