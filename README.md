# Dolores - Frontend Expert System

This is a demo of a frontend expert system with the Personal Data Protection Act (PDPA) example.

## Requirements
- NodeJS (only version 16 tested)

## Setup and Usage

### Using Docker

Configure `vue.config.js` such that the `publicPath` looks like the following:

```javascript
publicPath: process.env.NODE_ENV === 'production'
  ? '/'
  : '/',
```

This is because the `Dockerfile` is set to build the application for production with the assumption that it initally loads from root (i.e. `/`), but the Vue config file is configured for the purpose of building to Github pages. A fix will be made such that it can adapt to different deployment environments.

Then run:

```shell
$ docker build
$ docker-compose up
``` 

### Using Local Machine

Before starting the application, run the following set of install scripts:

```shell
$ npm install -g purescript spago
$ npm install
$ npx spago install
```

Start the application in development mode by running:

```shell
$ npm run serve
```

The application can be accessed at `localhost:8080`.

To compile and minify for distribution:

```shell
$ npm run build
```
