# Chapter 12: Math functions

This chapter introduces the **Math** library: trigonometry, powers and roots, exponentials and logarithms, rounding, min/max, and constants **π** and **e**. All angles are in **radians**. You import the functions you need from the **Math** module.

---

## 1. Importing the Math module

Import the functions you use. Constants are functions: **pi()** and **e()**.

```first
import { sin, cos, tan, sqrt, pow, exp, log, log10, abs, floor, ceil, round, sign, min, max, minInt, maxInt, pi, e } "Math"
import "Prelude"
```

---

## 2. Trigonometry (radians)

**sin**, **cos**, and **tan** take an angle in **radians**. Use **pi()** to convert from degrees: **radians = degrees * pi() / 180**.

### Example

```first
interaction main() -> Unit {
  println("sin(pi/2) = " + floatToString(sin(pi() / 2)));
  println("cos(0) = " + floatToString(cos(0)));
  println("tan(pi/4) = " + floatToString(tan(pi() / 4)));
  // 30 degrees -> radians
  let deg30 = 30 * pi() / 180;
  println("sin(30°) = " + floatToString(sin(deg30)));
}
```

---

## 3. Square root and power

**sqrt(x)** returns the square root of **x**. **pow(base, exp)** returns **base^exp**.

```first
interaction main() -> Unit {
  println("sqrt(2) = " + floatToString(sqrt(2)));
  println("sqrt(9) = " + floatToString(sqrt(9)));
  println("pow(2, 10) = " + floatToString(pow(2, 10)));
  println("pow(10, 2) = " + floatToString(pow(10, 2)));
}
```

---

## 4. Exponential and logarithm

**exp(x)** is **e^x**. **log(x)** is the natural logarithm; **log10(x)** is base-10.

```first
interaction main() -> Unit {
  println("e() = " + floatToString(e()));
  println("exp(1) = " + floatToString(exp(1)));
  println("log(e()) = " + floatToString(log(e())));
  println("log10(100) = " + floatToString(log10(100)));
}
```

---

## 5. Rounding and sign

**floor**, **ceil**, and **round** do what you expect. **abs(x)** is absolute value. **sign(x)** returns **-1**, **0**, or **1**.

```first
interaction main() -> Unit {
  println("floor(3.7) = " + floatToString(floor(3.7)));
  println("ceil(3.2) = " + floatToString(ceil(3.2)));
  println("round(3.5) = " + floatToString(round(3.5)));
  println("abs(-4.5) = " + floatToString(abs(-4.5)));
  println("sign(-10) = " + floatToString(sign(-10)));
  println("sign(0) = " + floatToString(sign(0)));
  println("sign(10) = " + floatToString(sign(10)));
}
```

---

## 6. Min and max

**min** and **max** work on **Float**; **minInt** and **maxInt** work on **Int**.

```first
interaction main() -> Unit {
  println("min(3.14, 2.71) = " + floatToString(min(3.14, 2.71)));
  println("max(3.14, 2.71) = " + floatToString(max(3.14, 2.71)));
  println("minInt(42, 17) = " + intToString(minInt(42, 17)));
  println("maxInt(42, 17) = " + intToString(maxInt(42, 17)));
}
```

---

## 7. Constants: pi and e

**pi()** returns π (≈ 3.14159…). **e()** returns e (≈ 2.71828…). They are functions so they work without module-level constants.

```first
interaction main() -> Unit {
  println("pi = " + floatToString(pi()));
  println("e = " + floatToString(e()));
  println("Area of circle r=5: " + floatToString(pi() * pow(5, 2)));
}
```

---

## 8. Putting it together

A short program that demonstrates several Math functions:

```first
import { sin, cos, pi, sqrt, pow, floor, round, min, maxInt } "Math"
import "Prelude"

interaction main() -> Unit {
  println("=== Trig ===");
  println("sin(pi/2) = " + floatToString(sin(pi() / 2)));
  println("cos(pi) = " + floatToString(cos(pi())));

  println("");
  println("=== Roots and powers ===");
  println("sqrt(49) = " + floatToString(sqrt(49)));
  println("2^8 = " + floatToString(pow(2, 8)));

  println("");
  println("=== Rounding ===");
  println("floor(2.9) = " + floatToString(floor(2.9)));
  println("round(2.5) = " + floatToString(round(2.5)));

  println("");
  println("=== Min/Max ===");
  println("min(1.0, 2.0) = " + floatToString(min(1.0, 2.0)));
  println("maxInt(10, 20) = " + intToString(maxInt(10, 20)));
}
```

You can run the full example from the **chapter-12-Math-functions** example project (see README).
