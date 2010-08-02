var result = (function () {
    function test() {
	return 5;
    }
    return test();
})();

assertobj(result === 5, "local_function");