# Dolores - Frontend Expert System

This is a demo of a frontend expert system with the Personal Data Protection Act (PDPA) example.

## Requirements
- NodeJS (versions 16 and above)

## Setup and Usage

### Configuring Environment Variables with `.env`

Before proceeding with the steps below, copy the `.env.example` to `.env`. Inside the `.env` file, configure the following settings according to your application:

```
BASE_URL=/
VUE_APP_BROWSER_NAME=CCLAW
VUE_APP_NAME=PDPA DBNO PoC - Draft
```

### Installing Packages

Run the following set of install scripts:

```shell
$ npm install
$ npx spago install
```

### Running in Development

Start the application in development mode by running:

```shell
$ npm run serve
```

The application can be accessed at `localhost:8080`.

### Building for Production

To build the application for production, run:

```shell
$ npm run build
```

A `dist/` directory should be produced. Deploy the contents in the `dist/` directory to your application of choice (e.g. Github pages).

An example can be found at <https://smucclaw.github.io/vue-pure-pdpa/> or <https://smucclaw.github.io/mengwong/pdpa/>.

### Using Docker

Alternatively, you may run this application on a Docker container by running:

```shell
$ docker-compose up --build
``` 
