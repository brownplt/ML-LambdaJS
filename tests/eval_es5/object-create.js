var o = {};
var o2 = Object.create(o);

assertobj(Object.getPrototypeOf(o2) === o, "object-create");