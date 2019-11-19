const fs = require('fs');
const jsf = require('json-schema-faker');
const tests = require('./tests.json');
var exec = require('child_process').execSync;

let getCoverge = (examples => {
    // first we write the tests to a file
    let json_str = JSON.stringify(examples)
    fs.writeFileSync('./test/examples.json', json_str, err => {
        if (err) throw err;
    });

    // then we run the coverge tests
    let execStdout = exec('npm test test/ex.js', {"encoding": "ascii"});
    console.log(execStdout);

    // then we go and get the coverge results
    let data = fs.readFileSync('./coverage/coverage-summary.json');

    let result = JSON.parse(data);
    return (result.total.lines.covered);
});

let data = tests.map(test => {
    // first we want to set up the file for doing tests
    fs.writeFileSync('./test/path.json', '"' + test.path + '"', err => {
        if (err) throw err;
    });

    // okay now need to generate examples from the "loose schema"
    var array = [];
    for (var i = 0; i < 10; i++) {
        array.push(jsf.generate(test.looseSchema));
    }

    // now we want to run the learner and get examples
    // .........

    // then we want to run each of the tests, and get their coverage
    return {
        description: test.description,
        example: getCoverge([test.start]),
        jsf: getCoverge(array)
    };
});

console.log(data);