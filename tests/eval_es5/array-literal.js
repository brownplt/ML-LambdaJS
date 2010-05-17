obj = {};
arr = ["1",2,obj];

assertobj(arr.length == 3 &&
	  arr[0] === "1" &&
	  arr[1] === 2 &&
	  arr[2] === obj,
	  "array-literal");
