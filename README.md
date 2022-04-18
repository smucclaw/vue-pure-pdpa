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

## Pushing to Production

This SPA is public at https://smucclaw.github.io/mengwong/pdpa/

That URL is served from the repo smucclaw/smucclaw.github.io.

	┌─[mengwong@rosegold] - [~/src/smucclaw/smucclaw.github.io] - [2022-04-18 02:29:20]
	└─[0] <git:(main 8bbd443✈) > tree mengwong/pdpa
	mengwong/pdpa
	├── css
	│   └── app.de92118c.css
	├── favicon.ico
	├── index.html
	└── js
		├── about.16319a60.js
		├── about.16319a60.js.map
		├── app.7a6408f6.js
		├── app.7a6408f6.js.map
		├── chunk-vendors.4ead29a5.js
		└── chunk-vendors.4ead29a5.js.map

	2 directories, 9 files

How do we update that "production" site?

In this directory, run `npm run build`

This should produce, under `dist/`, approximately the same as above, except the filenames will be different:

	┌─[mengwong@rosegold] - [~/src/smucclaw/vue-pure-pdpa] - [2022-04-18 02:33:40]
	└─[0] <git:(main eb9c758+✈) > tree dist
	dist
	├── css
	│   └── app.afd8b547.css
	├── favicon.ico
	├── index.html
	└── js
		├── about.78011b3e.js
		├── about.78011b3e.js.map
		├── app.9e1634ef.js
		├── app.9e1634ef.js.map
		├── chunk-vendors.e1032f08.js
		└── chunk-vendors.e1032f08.js.map

	2 directories, 9 files

sync `dist/` over to `../smucclaw.github.io/mengwong/pdpa/`

    rsync -va --delete dist/ ../smucclaw.github.io/mengwong/pdpa/

should do the trick

then go over to the `smucclaw.github.io` repo, and check in the new rev in the usual manner, something like:

    git add .
    git commit -m 'latest website'
    git push
	 
A minute or two later you should be able to reload `smucclaw.github.io/mengwong/pdpa/` and see your changes!

If the above procedure didn't work, you will have to do some
troubleshooting, I'm afraid. Go on Slack and complain that these
instructions didn't work, and if somebody else is around you might get
a second pair of eyes on the problem.

