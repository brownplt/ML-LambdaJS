o = {};

Object.freeze(o);

o.foo = 5;

/* foo should not be set */
assertobj(typeof o.foo === "undefined", "freeze2");
