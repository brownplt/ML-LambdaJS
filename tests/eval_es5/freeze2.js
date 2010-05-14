o = {};

Object.freeze(o);

o.foo = 5;

/* foo should not be set */
assert(typeof o.foo === "undefined", "freeze2");
