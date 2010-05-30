var o = {"foo":5};
var pd = Object.getOwnPropertyDescriptor(o,"foo");

assertobj((pd.configurable === true) &&
	  (pd.enumerable === true) &&
	  (pd.value == 5) &&
	  (pd.writable === true),
	  "getOwnPropertyDescriptor");
