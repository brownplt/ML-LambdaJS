ES5Harness = {

    global: window,

    registerTest: function(to) {
	this.test = new sth_test(to);
    },

    run : function () {
	var t      = this.test;   // the test
	var ut     = undefined;   // a particular unittest
	var res    = false;       // the result of running the unittest
	var prereq = undefined;   // any prerequisite specified by the unittest
	var pres   = true;        // the result of running that prerequite
	var cachedGlobal = this.global;
	var globalState = {
            undefined: cachedGlobal.undefined,
            NaN: cachedGlobal.NaN,
            Infinity: cachedGlobal.Infinity,
            Object: cachedGlobal.Object,
            Array: cachedGlobal.Array,
            Function: cachedGlobal.Function,
            String: cachedGlobal.String,
            Number: cachedGlobal.Number,
            Boolean: cachedGlobal.Boolean,
            RegExp: cachedGlobal.RegExp,
            Math: cachedGlobal.Math,
            Error: cachedGlobal.Error,
            eval: cachedGlobal.eval,
            parseInt: cachedGlobal.parseInt,
            parseFloat: cachedGlobal.parseFloat,
            isNaN: cachedGlobal.isNaN,
            isFinite: cachedGlobal.isFinite,
            EvalError: cachedGlobal.EvalError,
            RangeError: cachedGlobal.RangeError,
            ReferenceError: cachedGlobal.ReferenceError,
            SyntaxError: cachedGlobal.SyntaxError,
            TypeError: cachedGlobal.TypeError,
            URIError: cachedGlobal.URIError
        };

	ut = t;
	
	pre = ut.pre;
	pres = true;
	if (pre !== undefined) {
	    try {
		pres = pre();
		if (pres !== true) {
		    ut.res = 'Precondition failed';
		}
	    }
	    catch (e) {
		pres = false;
		ut.res = 'Precondition failed with exception: ' + e.description;
	    }
	}
	    
	try {
	    res = ut.theTestcase();
	    if (res === true) {
		print("Passed.");
		
	    }
	    else {
		print("Failed.");
	    }
	}
	catch (e) {
	    print('failed with exception: ' + e.name + ": " + e.message);
	}
    }
};

sth_test = function(to) {
    //Create a sth_test from a test definition object, and path
    //TODO:  Update sth framework to work more directly with test definitiion objects.
    this.testObj     = to;
    this.description = to.description;
    this.theTestcase = to.test;
    this.path        = path;
    this.res         = undefined;
    this.pre         = to.precondition;
};

// ----------------------------------------------
// helpers that unittests can use (typically in
// their prereq function).
// ----------------------------------------------
fnExists = function (f) {
  if (typeof(f) === "function") {
    return true;
  }
};

var supportsStrict = undefined;
fnSupportsStrict = function () {
   "use strict";
   if (supportsStrict!==undefined) return supportsStrict;
   try {eval('with ({}) {}'); supportsStrict=false;} catch (e) {supportsStrict=true;};     
   return supportsStrict;
};

fnGlobalObject = function () {
  return (function () {return this;}).call(null);
};


compareArray = function (aExpected, aActual) {
  if (aActual.length != aExpected.length) {
    return false;
  }

  aExpected.sort();
  aActual.sort();

  var s;
  for (var i = 0; i < aExpected.length; i++) {
    if (aActual[i] != aExpected[i]) {
      return false;
    }
  }
  
  return true;
};
