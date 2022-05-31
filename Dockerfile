# Build
FROM node:16 as build-stage

WORKDIR /app
COPY package*.json *.dhall ./

RUN npm install
RUN npx spago install

COPY . .
RUN npm run build

# Production
FROM nginx as production-stage

COPY --from=build-stage /app/dist /usr/share/nginx/html

EXPOSE 80
CMD [ "nginx", "-g", "daemon off;" ]
