FROM node:latest

WORKDIR /app

COPY ["package.json", "style.css", "main.js", "slides/", "index.html", "./"]

RUN npm config set registry="http://registry.npmjs.org/"
RUN npm install

EXPOSE 3000

CMD ["npm", "run", "dev"]