'use strict';
const jsf = require('json-schema-faker')
const express = require('express');
const fs = require('fs');
const bodyParser = require('body-parser');

//create a server object:
const app = express()
app.use(bodyParser.text({ type: 'text/plain'})); 

function randelem(examples) {
    return examples[Math.floor(Math.random()*examples.length)];
}


// https://stackoverflow.com/questions/25745206/using-javascripts-filter-function
let t2 = {
    s: [1, "h", [], {"h":3}],
    o: val => !(val === "" || typeof val == "undefined" || val === null),
    e: () => {
        let v = randelem([
            [1, "h", [], {"h":3}],
            [1, "h", [], {"h":3}]
        ]);
        return v;
    }
}

// https://github.com/sveltejs/svelte/blob/dda9a53727c3b5d635dac4db5f251f0a878104b0/src/compiler/compile/render_ssr/index.ts
let t3 = {
    s: {name: ["$", "$"]},
    o: (variable => variable.name[0] === '$' && variable.name[1] !== '$'),
    e: () => {
        let v = randelem([
            {name: ["$", "$", "$"]},
            {name: ["$", "$", "a"]}
        ]);
        return v;
    }
}

let t4 = {
    s: {type: "svelte"},
    o: x => x.type === 'svelte' || x.type === 'js',
    e: () => {
        let v = randelem([
            {name: ["$", "$", "$"]},
            {name: []}
        ])
    }
}

const t = t2;

app.get('/start', function(_, resp) {
    resp.json(t.s);
})

app.get('/example', function (_, resp) {
    resp.json(t.e());
})

app.post('/member', function (req, resp) {
    try {
        let v = JSON.parse(req.body);
        t.o(v);
        resp.json(true);
    } catch (msg) {
        resp.json(false);
    }
})

app.listen(8080)


// console.log(value);