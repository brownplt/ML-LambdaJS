var x = '\b';
var z = {
  '\b': '\\b' // lhs is a PropString, not a StringLit, but must be printed
              //  as a StringLit
};
var w = "\\s";
