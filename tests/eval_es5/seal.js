o = {};

Object.seal(o);

o.foo = 5;

/* foo should not be set */
assertobj(typeof o.foo === "undefined", "seal");
