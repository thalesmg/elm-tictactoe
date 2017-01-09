var _user$project$Native_MyLazy = function() {

function memoize(thunk) {
        var values = {};
        var isForced = {};
        return function(tuple0) {
            // console.log(tuple0);
            if (!isForced[hashGame(tuple0)]) {
                values[hashGame(tuple0)] = thunk(tuple0);
                // console.log(values[tuple0])
                isForced[hashGame(tuple0)] = true;
            }
            return values[hashGame(tuple0)];
        };
    }

function hashArray(array) {
    var out = [];
    var roww, elemm;
    for (roww in array.table) {
        for (elemm in array.table[roww].table) {
            if (array.table[roww].table[elemm].ctor == "Nothing") {
                out.push("Nothing");
            } else {
                out.push("Just " + array.table[roww].table[elemm]._0.ctor);
            }
        }
    }
    return "[" + out.join(',') + "]";
}

function hashGame(thunk) {
    return "(" + thunk._0.ctor + "," + thunk._1 + "," + hashArray(thunk._2) + ")";
}

return {
    memoize: memoize
};

}();