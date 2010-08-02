var result = (function testcase() {
  'use strict';
  try 
  {
    arguments.callee;
  }
  catch (e) {
    if(e instanceof TypeError)
      return true;
  }
})();

assertobj(result, "callee");
