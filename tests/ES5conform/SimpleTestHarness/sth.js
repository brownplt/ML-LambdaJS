/// Copyright (c) 2009 Microsoft Corporation 
/// 
/// Redistribution and use in source and binary forms, with or without modification, are permitted provided
/// that the following conditions are met: 
///    * Redistributions of source code must retain the above copyright notice, this list of conditions and
///      the following disclaimer. 
///    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
///      the following disclaimer in the documentation and/or other materials provided with the distribution.  
///    * Neither the name of Microsoft nor the names of its contributors may be used to
///      endorse or promote products derived from this software without specific prior written permission.
/// 
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
/// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
/// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
/// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
/// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
/// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
/// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
/// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

/*
sth: Simple Test Harness
*/


sth.prototype.registerTest = function (to) {
  // registerTest is the method that test definitions call to register test definition objects
  var registrationPath = to.path;
  var validatedPath = '<I>missing path</I>'
  var id = to.id;
  var t = new sth_test(to);
  t.registrationIndex = this.tests.length;
  this.tests.push(t);  //list of tests in registration order
  if (!registrationPath && !id) {
     this.println('Test case #'+(t.registrationIndex+1) +' missing both id and path properties.');
     return;
     }
  if (registrationPath && id &&registrationPath.slice(registrationPath.lastIndexOf('/')+1,-3) !== id) 
     this.println('Test case with mismatched id ('+id+') and path ('+ registrationPath+') properties.');
  if (registrationPath) {
     validatedPath = registrationPath;
     if (this.testsByPath[registrationPath]) 
        this.println('Duplicate registration path '+ registrationPath);
     else this.testsByPath[registrationPath] = t;
     }
  if (id) {
    if (this.testsById[id]) 
      this.println('Duplicate test id '+ id + ' in files ' +registrationPath+' and '+ this.testsById[id].testObj.path);
    else this.testsById[id]=t;
    }
  };

sth.prototype.startTesting = function () {
  // Start running the registered tests.  When done, generate the report.
  this.prepareToTest();
  this.run();
  this.report();
  };

sth.prototype.sectionBreak = function () {
  // used to identify a break between groups of tests
  this.add(null);
  };


sth.prototype.run = function () {
  var t      = this.tests;  // the tests collection
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
          }

  function restoreGlobals() {
    for (var prop in globalState) 
        if (cachedGlobal[prop] !== globalState[prop]) cachedGlobal[prop] = globalState[prop];
    }
  for (var i = 0; i < t.length; i++) {
    this.startingTest(ut = t[i]);

    // if the test specifies a prereq, run that.
    pre = ut.pre;
    pres = true;
    if (pre !== undefined) {
      try {
        pres = pre.call(ut.testObj);
        restoreGlobals();
        if (pres !== true) {
          ut.res = 'Precondition failed';
        }
      }
      catch (e) {
        restoreGlobals();
        pres = false;
        ut.res = 'Precondition failed with exception: ' + e.description;
      }
    }
    
    // if the prereq is met, run the testcase now.
    if (pres === true) {
      try {
        res = ut.theTestcase.call(ut.testObj);
        restoreGlobals();
        if (res === true) {
          ut.res = 'pass';
          this.totalTestsPassed++;
        }
        else {
          ut.res = 'fail';
        }
      }
      catch (e) {
        restoreGlobals();
        ut.res = 'failed with exception: ' + e.description;
      }
    }

    this.totalTestsRun++;
  }
}

sth.prototype.matchTestPath = function (filePath) {
   var cannonicalPath = filePath.slice(filePath.indexOf('TestCases'));
   var possibleMatch = this.testsByPath[cannonicalPath];
   if ( possibleMatch) return possibleMatch;
   var pathId = filePath.slice(filePath.lastIndexOf('/')+1, -3);
   possibleMatch = this.testsById[pathId];
   if ( possibleMatch) return possibleMatch;
   return null;
   }
   
sth.prototype.htmlEscape = function htmlEscape(str) {
    str = str.replace(/</g,'&lt;');
    return str.replace(/>/g,'&gt;');
    }

 
sth.prototype.OpenSourceWindow = function (idx) {
  var ut = this.tests[idx];
  var popWnd = window.open("", "", "scrollbars=1, resizable=1");
  var innerHTML = '';

  innerHTML += '<b>Test </b>';
  if (ut.testObj.id) innerHTML += '<b>' + ut.testObj.id + '</b> <br><br>';
  
 if (ut.description) {
    innerHTML += '<b>Description</b>';
    innerHTML += '<pre>' + this.htmlEscape(ut.description) + ' </pre>';
  }

  innerHTML += '<b>Testcase</b>';
  innerHTML += '<pre>' + ut.theTestcase + '</pre>';

  if (ut.pre) {
    innerHTML += '<b>Precondition</b>';
    innerHTML += '<pre>' + ut.pre + '</pre>';
  }

  innerHTML += '<b>Path</b>';
  innerHTML += '<pre>' + ut.path + ' </pre>&nbsp';
  popWnd.document.write(innerHTML);
}

sth.prototype.report = function sth_report() {
  this.resultsDiv.innerHTML = '';  // clear ouput
  this.println('Total tests: '+ requestedTests +
               ' Passed: ' + this.totalTestsPassed +
               ' Failed: '+ (this.tests.length-this.totalTestsPassed) +
               ' Could not load: ' + (requestedTests - this.tests.length));
   this.println('');

  var t = aryTestCasePaths;
  var utPath;
  var ut = undefined;
  var utId;
  var thisSth = this;
    
  function testline(ut) {
    ut.printed = true;
    if (ut.testObj.id) utId=ut.testObj.id;
    else utId=ut.testObj.path.slice(ut.testObj.path.lastIndexOf('/')+1,-3);
    var href = '<span style="color:blue" onclick="activeSth.OpenSourceWindow(' +ut.registrationIndex+');"> [Source] </span>';

    if (ut.res === 'pass') thisSth.println(ut.testObj.id+' '+thisSth.htmlEscape(ut.description) + ': <span style=\"color:green\">'+ ut.res + '</span>'+ href);
    else thisSth.println(utId+' '+thisSth.htmlEscape(ut.description) + ': ' + '<span style=\"color:red\">' + ut.res + '</span>' + href);
    }

  for (var i = 0; i < t.length; i++) {
    utPath = t[i];

    if (!utPath ) {
      this.println('');
      continue;
      }
    ut = this.matchTestPath(utPath);
    if (ut) {
       ut.path=utPath;
       testline(ut);
       }
    else {
       var idFromFilePath = utPath.slice(utPath.lastIndexOf('/')+1,-3);
       if (idFromFilePath.charAt(idFromFilePath.length-1).toLowerCase() ==='x')
          this.println(idFromFilePath+' <I>Intentional syntax error </I>: <span style=\"color:green\">pass</span> ' );
       else this.println(utPath+'<span style=\"color:red\">' + ' Missing test: either misidentifed or could not load because of unexpected syntax error' + '</span>');
       }
  }
  
  t=this.tests;
  var missingHeaderNeeded = true;
  for (var ix = 0; ix < t.length; ix++) {
     ut = t[ix];
     if (ut.printed)continue;
     if (missingHeaderNeeded) {
        this.println('<span>&nbsp;</span>');
        this.println('The following tests are internally misidentified and could not be matched to requested test files:');
        missingHeaderNeeded = false;
        }
      testline(ut);
      }

  this.resultsDiv.innerHTML += this.innerHTML;
}

sth.prototype.println = function (s) {
  this.innerHTML += s;
  this.innerHTML += "<BR/>";
}

sth.prototype.prepareToTest = function (s) {
  this.resultsDiv = document.createElement("div");  
  document.body.appendChild(this.resultsDiv);
  for (var i = 0; i < aryTestCasePaths.length; i++) {
    if (aryTestCasePaths[i])requestedTests++;
  }
}

sth.prototype.startingTest= function(test) {
    this.currentTest = test;
    this.resultsDiv.innerHTML = 'current test ' +test.testObj.id+ ': '+test.description;
    }
    
function sth(globalObj) {
  this.global           = globalObj;
  this.totalTestsRun    = 0;
  this.totalTestsPassed = 0;
  this.tests            = [];
  this.testsByPath      = {};
  this.testsById        = {};
  this.unidentifedTests = [];
  this.innerHTML        = "";
  this.resultsDiv       = null
}

function sth_test(to,  path) {
  //Create a sth_test from a test definition object, and path
  //TODO:  Update sth framework to work more directly with test definitiion objects.
  this.testObj     = to;
  this.description = to.description;
  this.theTestcase = to.test;
  this.path        = path;
  this.res         = undefined;
  this.pre         = to.precondition;
}

//define 
var activeSth = new sth(window);

var aryTestCasePaths;
var testIndex = 0;
var requestedTests = 0;

function ConvertToFileUrl(pathStr) {
  return "file:" + pathStr.replace(/\\/g, "/");
}

function sth_loadtests(aryPaths, testHarnessPath) {

  function addScriptElement(url) {
     var script = document.createElement("script");
     document.body.appendChild(script);
     script.src = url;
  }
  
  aryTestCasePaths = aryPaths;  //should already be the case
  var thPath = testHarnessPath ? testHarnessPath : '.';

  for (var i = 0; i < aryPaths.length; i++) {
    if (aryPaths[i]) {
       addScriptElement(ConvertToFileUrl(aryPaths[i]));
       requestedTests++;
       }
  }
  addScriptElement("file:"+thPath+"/starttesting.js");
}


// ----------------------------------------------


// ----------------------------------------------
// helpers that unittests can use (typically in
// their prereq function).
// ----------------------------------------------
function fnExists(f) {
  if (typeof(f) === "function") {
    return true;
  }
}

var supportsStrict = undefined;
function fnSupportsStrict() {
   "use strict";
   if (supportsStrict!==undefined) return supportsStrict;
   try {eval('with ({}) {}'); supportsStrict=false;} catch (e) {supportsStrict=true;};     
   return supportsStrict;
  }

function fnGlobalObject() {
  return (function () {return this}).call(null);
  }


function compareArray(aExpected, aActual) {
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
}
