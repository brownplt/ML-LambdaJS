var a = [1,2];
var b = a.slice(0);

assertobj(b.length === 2 &&
	  b[0] === 1 &&
	  b[1] === 2,
	  "slice1");

var c = [1,2,3];
var d = c.slice(0,1);

assertobj(d.length === 1 &&
	  d[0] === 1,
	  "slice2");

var e = [1,2,3,4,5];
var f = e.slice(-2);

assertobj(f.length === 2 &&
	  f[0] === 4 &&
	  f[1] === 5,
	  "slice3");
