# Build
FROM node:16 as build-stage

WORKDIR /app
COPY package*.json *.dhall ./

RUN npm install -g npm spago purescript
RUN npm run deps

COPY . .
RUN npm run build

# Production
FROM nginx:3 as production-stage

COPY --from=build-stage /app/dist /usr/share/ngnix/html

EXPOSE 8080
CMD [ "ngnix", "-g", "daemon off;" ]
