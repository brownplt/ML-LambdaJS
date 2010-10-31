var o = {get a() { return 5; }};

assertobj(o.a === 5, "getter1");

var o2 = {_a : 10, get a() { return this._a; }, set a(v) { this._a = v; }};

var old_a = o2.a;

o2.a = "str";

assertobj(o2.a === "str" && old_a === 10, "getter2");
