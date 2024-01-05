```
            __
           /\ \
 __  __  __\ \ \___   __  __
/\ \/\ \/\ \\ \  _ `\/\ \/\ \
\ \ \_/ \_/ \\ \ \ \ \ \ \_\ \
 \ \___x___/' \ \_\ \_\/`____ \
  \/__//__/    \/_/\/_/`/___/> \
                          /\___/
                          \/__/
```

# (wh)y lang

**language example**

```y
let a = 0;
let b = "str";
let c = true;
let test = fn x: num, y: str, z: bool -> str {
  "num:" + x + " str:" + y + " bool:" + z
};

test(a, b, c)

let wrapper = fn inner: (fn a: number -> number) -> number {
  inner(20)
}

wrapper({
  a * 4
})
```
