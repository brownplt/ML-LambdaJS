var a = new Array();

a[0] = "foozle";
a[5] = "asdF";

assertobj(a.join(":") == "foozle:::::asdF", "array-join");