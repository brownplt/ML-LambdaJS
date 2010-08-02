var result = (function testcase() {
  'use strict';
  try 
  {
    arguments.caller;
  }
  catch (e) {
    if(e instanceof TypeError)
      return true;
  }
})();

assertobj(result, "caller");
