o = {foo: 5};

op_old = Object.getOwnPropertyDescriptor(o, "foo");

Object.seal(o);

op = Object.getOwnPropertyDescriptor(o, "foo");

assert(op_old.configurable === true &&
       op_old.value === 5 &&
       op_old.writable === true &&
       op_old.enumerable === true &&
       op.configurable === false &&
       op.value === 5 &&
       op.writable === true &&
       op.enumerable === true, "seal2");