let a = 0;
let b = "str";
let c = true;
let test = fn x: num, y: str, z: bool -> str {
  "num:" + x + " str:" + y + " bool:" + z
};

test(a, b, c)