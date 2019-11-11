function littleFunction(object) {
    let v = (object.x.reduce((x, y) => x+y, 0))/(object.x.size)
    if (isNaN(v)){
        throw "toobig";
    } else {
        console.log(v);
    }
}

function oracle(trace, little) {
    return object => {
        try {
            little(object);
        } catch (error) {
            console.log(error);
            return error === trace;
        }
        return false;
    }
}

o = oracle("toobig", littleFunction);

for (i = 0; i < 10; i++) {
    console.log(o({x: []}));
}