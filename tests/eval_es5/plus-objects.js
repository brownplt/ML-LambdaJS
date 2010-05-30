/* Tests for desugaring to [[plus]] */

test = function() {

    /* Since the preferred type for objects ToPrimitive is number,
     * these should all add numbers */
    return ((5 + { valueOf: function() { return 6; }} == 11) &&
	    (5 + { valueOf: function() { return "6"; }} == 11) &&
	    ("5" + { toString: function() { return 6; }} == "56"));
};

assertobj(test(), "plus-objects.js");