'use strict';
// import jsf from 'json-schema-faker';
const express = require('express')

//create a server object:
const app = express()

app.get('/', function (req, esp,) {
    res.send("Hello, world!");
})

// const schema = {
//     type: "string"
// };

// const value = jsf.generate(schema);
// console.log(value);