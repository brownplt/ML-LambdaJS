
function() {

var o = { toString: function() {return "toString called";}};
var f = o.toString;

assertobj(f() === "toString called", "get-and-call");
}();