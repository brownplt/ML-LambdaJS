var result = (function testcase() {
  function foo(a,b,c)
  {
    a = 1; b = 'str'; c = 2.1;
    if(arguments[0] === 10 && arguments[1] === 'sss' && arguments[2] === 1)
      return true;   
  }
  return foo(10,'sss',1);
})();


assertobj(result, "10.6");