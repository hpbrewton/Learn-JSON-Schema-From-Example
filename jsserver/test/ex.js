// test/hello-world.js
const path = require('./path.json');
const examples = require('./examples.json');
const alg = require(path).default;
// const readline = require('readline');
const tap = require('tap');

console.log(alg);
examples.forEach(v => {
    try {
        if (alg.length == 1) {
            tap.pass(alg(v));
        } else {
            tap.pass(alg(...v));
        }
    } catch (msg) {
        console.log(msg);
        ;
    }
});



// tap.pass(t1.t1.o([]));
// tap.pass(t1.t1.e());




