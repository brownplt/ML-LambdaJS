var a = Array();
a.length = 10;

assertobj(a.length === 10, "array-length1");

var b = new Array();
b[0] = "val";

assertobj(b.length === 1, "array-length2");

var c = new Array();
c[12] = "foo";

assertobj(c.length === 13, "array-length3");