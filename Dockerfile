# Build
FROM node:16 as build-stage

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y libncurses5

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
