# vue-pure-pdpa

## Project setup
```
npm install
npx spago install
```

### Compiles and hot-reloads for development
```
npm run serve
```

### Compiles and minifies for production
```
npm run build
```

### Run your unit tests
```
npm run test:unit
```

### Run your end-to-end tests
```
npm run test:e2e
```

### Run purescript tests
```
npx spago test
```

### Lints and fixes files
```
npm run lint
```

### Customize configuration
See [Configuration Reference](https://cli.vuejs.org/config/).

### FAQ

```
error  in ./src/index.purs

Syntax Error: Error: compilation failed:
From previous event:
```
- running spago test will produce `.spago` and `output/` which will cause errors for npm for some reason,
- someone should investigate why
- current workaround `npm run clean:purs`
