o = {foo: 5};

op_old = Object.getOwnPropertyDescriptor(o, "foo");

Object.freeze(o);

op = Object.getOwnPropertyDescriptor(o, "foo");

assertobj(op_old.configurable === true &&
	  op_old.value === 5 &&
	  op_old.writable === true &&
	  op_old.enumerable === true &&
	  op.configurable === false &&
	  op.value === 5 &&
	  op.writable === false &&
	  op.enumerable === true, "freeze1");
