var o = {};
try {
    Object.defineProperty(o,"foo", {value: 10, get: function() {}});
}
catch(e) {
    assertobj(e instanceof TypeError, "defineProperty1");
}

try {
    Object.defineProperty(o, "foo", {value: 4, set: function() {}});
}
catch(e) {
    assertobj(e instanceof TypeError, "defineProperty2");
}

Object.defineProperty(o, "foo", {value: 10, configurable: false, writable: false});
var prop = Object.getOwnPropertyDescriptor(o,"foo");

assertobj(prop.configurable === false &&
	  prop.value === 10 &&
	  prop.writable === false,
	  "defineProperty3");
