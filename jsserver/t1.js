function randelem(examples) {
    return examples[Math.floor(Math.random()*examples.length)];
}

let t1 = {s: [1, 2, 3],
    o: function(numbers){
        var average = 0;
        var total = 0;
        for (var i = 0 ; i < numbers.length; i++){    
            total += numbers[i];   //add the each number in the array to the cumulative total 
        }
        average = total/i;
        if (isNaN(average)) {
            return false;
        }
    },
    e: () => {
        let v = randelem([
            [1, 2, 3],
            [1, 3]
        ])
        // console.log(v);
        return v 
    }
}
module.exports.t1 = t1;