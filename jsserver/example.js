// can we even access files in javascript-aglorithms
import degreeToRadian from './javascript-algorithms/src/algorithms/math/radian/degreeToRadian.js';

let schema = {
    "type": "object",
    "properties": {
        "degree": {
            "type": "number"
        }
    }
}

console.log(degreeToRadian(12));