/* Tests for desugaring to [[plus]] */

test = function() {

    return ((5 + "5" === "55") &&
	    ("5" + 5 === "55") &&
	    (5 + 5 == 10));

};

assert(test(), "plus-simple");