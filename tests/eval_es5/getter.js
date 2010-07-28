var o = {get a() { return 5; }};

assertobj(o.a === 5, "getter1");

var o2 = {a : 10, set a(v) { this.a = 12; }};

var old_a = o2.a;

o2.a = "str";

assertobj(o2.a === "str" && old_a === 10, "getter2");
