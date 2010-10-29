function() {
    try {
        var err = function ()  { throw "error"; };
        var y = 0;
        var o = {x : (y = 5), z: err()};
    }
    catch(e) {
        assertobj(y === 5 &&
                  e === "error",
                 "eval_order1");
    }
}();

function() {
    try {
        var err = function ()  { throw "error"; };
        var y = 0;
        var o = {x : err(), z: (y = 5)};
    }
    catch(e) {
        assertobj(y === 0 &&
                  e === "error",
                 "eval_order2");
    }
}();

function() {
    try {
        var err = function ()  { throw "error"; };
        var doNothing = function() {};
        var y = 0;
        doNothing(err(), (y = 5));
    }
    catch(e) {
        assertobj(y === 0 &&
                  e === "error",
                 "eval_order3");
    }
}();

function() {

    var err = function ()  { throw "error"; };
    var doNothing = function() {};
    var y = 0;
    doNothing((y = 5), (y = 6));
    assertobj(y === 6,
              "eval_order4");

}();

