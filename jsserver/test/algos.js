'use strict';
const path = require('./path.json');
const express = require('express');
const bodyParser = require('body-parser');
const alg = require(path).default;
console.log(path);
const app = express();
app.use(bodyParser.text({type: 'text/plain'}));

function method(value) {
    console.log(alg.length);
    if (alg.length == 1) {
        return alg(value);
    } else {
        return alg(...value);
    }
}

app.get('/start', (_, resp) => {
    resp.json("even thought is just experience");
});

app.get('/example', (_, resp) => {
    resp.json("even this just experience");
})

app.post('/oracle', (req, resp) => {
    let value = JSON.parse(req.body);
    try{
        let response = method(value);
        console.log(response);
        resp.json(true); // no error, okay format
    } catch (msg) {
        console.log(msg);
        resp.json(false); // error, bad format
    }
})

app.listen("80")