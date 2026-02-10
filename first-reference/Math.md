# Math

The Math library provides common mathematical functions and constants. All angles are in **radians**.

**Import:** `import "Math"` or `import { sin, cos, pi, sqrt } "Math"`

---

## Trigonometry

| Function | Type | Description |
|----------|------|--------------|
| `sin(x)` | `Float -> Float` | Sine |
| `cos(x)` | `Float -> Float` | Cosine |
| `tan(x)` | `Float -> Float` | Tangent |

---

## Power and roots

| Function | Type | Description |
|----------|------|--------------|
| `sqrt(x)` | `Float -> Float` | Square root |
| `pow(base, exp)` | `(Float, Float) -> Float` | base^exp |

---

## Exponential and logarithm

| Function | Type | Description |
|----------|------|--------------|
| `exp(x)` | `Float -> Float` | e^x |
| `log(x)` | `Float -> Float` | Natural log |
| `log10(x)` | `Float -> Float` | Base-10 log |

---

## Rounding and sign

| Function | Type | Description |
|----------|------|--------------|
| `abs(x)` | `Float -> Float` | Absolute value |
| `floor(x)` | `Float -> Float` | Round down |
| `ceil(x)` | `Float -> Float` | Round up |
| `round(x)` | `Float -> Float` | Round to nearest |
| `sign(x)` | `Float -> Float` | -1, 0, or 1 |

---

## Min / max

| Function | Type | Description |
|----------|------|--------------|
| `min(a, b)` | `(Float, Float) -> Float` | Smaller of two floats |
| `max(a, b)` | `(Float, Float) -> Float` | Larger of two floats |
| `minInt(a, b)` | `(Int, Int) -> Int` | Smaller of two ints |
| `maxInt(a, b)` | `(Int, Int) -> Int` | Larger of two ints |

---

## Constants

| Function | Type | Description |
|----------|------|--------------|
| `pi()` | `() -> Float` | π (≈ 3.14159…) |
| `e()` | `() -> Float` | e (≈ 2.71828…) |

Example: after `import { sin, pi } "Math"`, use `sin(pi() / 2)` → `1.0`.
