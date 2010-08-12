var x = 0;
(function testcase() {
    for(var i = 0; i<10; i++) {
	x = x+5;
    }
})();

assertobj(x == 50, "for-loop");