const {resolve} = require('path')
const {defineConfig} = require('vite')

const lectures = require('fast-glob')
    .sync(['./slides/**/*.html', '!dist'])
    .map(entry => resolve(__dirname, entry));

lectures.push(resolve(__dirname, 'index.html'));
console.log(lectures);

module.exports = defineConfig({
    build: {
        rollupOptions: {
            input: lectures
        },
    },
    base: "/lectures/"
})