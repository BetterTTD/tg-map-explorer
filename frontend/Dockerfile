FROM node:lts AS development

ENV CI=true
ENV PORT=3000

WORKDIR /code
COPY package.json /code/package.json
COPY package-lock.json /code/package-lock.json

RUN npm ci

COPY . /code

CMD npm run build

FROM development AS builder

FROM nginx:1.13-alpine

COPY --from=builder /code/dist /usr/share/nginx/html