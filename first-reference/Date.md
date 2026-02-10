# Date

The Date library works with dates and times. A date is represented as an **Int** handle (Unix timestamp in seconds). Use **0** to mean “invalid” or “no date”.

**Import:** `import "Date"` or `import { now, format, parse } "Date"`

---

## Getting the current time

| Function | Type | Description |
|----------|------|--------------|
| `now()` | `() -> Int` | Current time (UTC) as a date handle |

---

## Formatting and parsing

| Function | Type | Description |
|----------|------|--------------|
| `format(d, fmt)` | `(Int, String) -> String` | Format a date with a strftime-style format (e.g. `"%Y-%m-%d %H:%M:%S"`). Returns a new string; caller does not free. |
| `parse(s)` | `String -> Int` | Parse `"YYYY-MM-DD"` or `"YYYY-MM-DDTHH:MM:SS"`. Returns 0 on failure. |

---

## Getters (UTC)

All getters take a date handle and return the component. Behaviour is undefined if the handle is 0.

| Function | Type | Description |
|----------|------|--------------|
| `getYear(d)` | `Int -> Int` | Year (e.g. 2025) |
| `getMonth(d)` | `Int -> Int` | Month 1–12 |
| `getDay(d)` | `Int -> Int` | Day of month 1–31 |
| `getHours(d)` | `Int -> Int` | Hour 0–23 |
| `getMinutes(d)` | `Int -> Int` | Minute 0–59 |
| `getSeconds(d)` | `Int -> Int` | Second 0–59 |

---

## Arithmetic

| Function | Type | Description |
|----------|------|--------------|
| `addSeconds(d, seconds)` | `(Int, Int) -> Int` | Add (or subtract) seconds. Returns a new date handle. |

---

## Example

```first
import { now, format, getYear } "Date"
import "Prelude"

interaction main() -> Unit {
  let d = now();
  println(format(d, "%Y-%m-%d %H:%M:%S"));
  println(intToString(getYear(d)));
}
```
