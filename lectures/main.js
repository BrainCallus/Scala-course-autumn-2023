import 'reveal.js/dist/reveal.css'
// see available themes in the
// node_modules/reveal.js/dist/theme
//  beige, black, blood, league, moon, night, serif, simple, ...
import 'reveal.js/dist/theme/white.css'
import 'reveal.js/plugin/highlight/monokai.css'

import Reveal from 'reveal.js'
import Markdown from 'reveal.js/plugin/markdown/markdown.esm.js';
import Highlight from 'reveal.js/plugin/highlight/highlight.esm.js';

let deck = new Reveal({
    plugins: [ Markdown, Highlight ]
})
deck.initialize({ hash: true, slideNumber: true })