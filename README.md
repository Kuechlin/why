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

```
a = 0;
b = "str";
c = true;
test = fn x: num, y: str, z: bool -> str {
  "num:" + x + " str:" + y + " bool:" + z
};

test(a, b, c)

wrapper = fn inner: (fn a: num -> num) -> num {
  inner(20)
};

wrapper({
  a * 4
})
```
