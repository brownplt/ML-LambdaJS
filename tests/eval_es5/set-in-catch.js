var result = (function testcase() {
  foo = "prior to throw";
  try {
    throw new Error();
  }
  catch (foo) {
    var foo = "initializer in catch";
  }
 return foo === "prior to throw";
  
})();

assertobj(result, "set-in-catch");
