let djikstra = (graph, start) => {
    let frontier = [];
    let nodes = graph.topo.reduce(
            (x, y) => x.concat(y),
            []
        )
        .filter(x => 'string' === typeof x)
        .filter((v, i, a) => a.indexOf(v) == i)
        ;
    
    let adjacency = graph.topo.reduce(
            (x, y) => {
                if (!(x.hasOwnProperty(y[0]))) {
                    x[y[0]] = []
                }
                x[y[0]].push(y[1]);
                return x;
            },
            {}
        )
        ;
    let distance = {};
    let previous = {};
    nodes.forEach(node => {
        distance[node] = Infinity;
        previous[node] = undefined;
        frontier.push(node);
    })

    distance[start] = 0;
    while(frontier.size != 0) {
        v = frontier
    }
    console.log(distance);
}


exports['default'] = djikstra;