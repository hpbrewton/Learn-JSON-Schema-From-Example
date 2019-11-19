let trajan = (graph) => {
    var index = 0;
    var nodes = graph.topo.reduce(
            (x, y) => x.concat(y),
            []
        )
        .filter(x => 'string' === typeof x)
        .filter((v, i, a) => a.indexOf(v) == i)
        .map(v => {return {"name": v, "index": undefined}})
        ;
    
    console.log(nodes);
    return nodes;
}

exports['default'] = trajan;