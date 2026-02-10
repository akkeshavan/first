# Chapter 11: Date functions

This chapter introduces the **Date** library: getting the current time, formatting and parsing dates, reading components (year, month, day, etc.), and simple arithmetic. Dates are represented as **Int** handles (Unix timestamps in seconds). You import the functions you need from the **Date** module.

---

## 1. Importing the Date module

Import specific functions, or the whole module. Dates are **Int** handles; use **0** to mean invalid or “no date”.

```first
import { now, format, parse, getYear, getMonth, getDay, getHours, getMinutes, getSeconds, addSeconds } "Date"
import "Prelude"
```

---

## 2. Current time and formatting

**now()** returns the current time (UTC) as a date handle. **format(d, fmt)** turns that handle into a string using a **strftime-style** format.

### Common format codes

| Code | Meaning | Example |
|------|---------|---------|
| `%Y` | 4-digit year | 2025 |
| `%m` | Month 01–12 | 03 |
| `%d` | Day of month 01–31 | 15 |
| `%H` | Hour 00–23 | 14 |
| `%M` | Minute 00–59 | 30 |
| `%S` | Second 00–59 | 45 |

### Example

```first
interaction main() -> Unit {
  let d = now();
  println("Now (ISO-style): " + format(d, "%Y-%m-%d %H:%M:%S"));
  println("Date only: " + format(d, "%Y-%m-%d"));
  println("Time only: " + format(d, "%H:%M:%S"));
}
```

---

## 3. Parsing a date string

**parse(s)** reads a date from a string. It accepts:

- **"YYYY-MM-DD"** (e.g. `"2025-03-15"`)
- **"YYYY-MM-DDTHH:MM:SS"** (e.g. `"2025-03-15T14:30:00"`)

It returns a date handle, or **0** on failure.

```first
interaction main() -> Unit {
  let d = parse("2025-12-25");
  if (d != 0) {
    println("Parsed: " + format(d, "%Y-%m-%d"));
  } else {
    println("Parse failed");
  };
  let d2 = parse("2025-06-15T09:00:00");
  println(format(d2, "%Y-%m-%d %H:%M:%S"));
}
```

---

## 4. Getters (year, month, day, time)

All getters take a date handle and return an **Int**. They use **UTC**.

| Function | Returns |
|----------|---------|
| `getYear(d)` | Year (e.g. 2025) |
| `getMonth(d)` | Month 1–12 |
| `getDay(d)` | Day of month 1–31 |
| `getHours(d)` | Hour 0–23 |
| `getMinutes(d)` | Minute 0–59 |
| `getSeconds(d)` | Second 0–59 |

### Example

```first
interaction main() -> Unit {
  let d = now();
  println("Year: " + intToString(getYear(d)));
  println("Month: " + intToString(getMonth(d)));
  println("Day: " + intToString(getDay(d)));
  println("Time: " + intToString(getHours(d)) + ":" + intToString(getMinutes(d)) + ":" + intToString(getSeconds(d)));
}
```

---

## 5. Adding seconds

**addSeconds(d, n)** returns a new date handle that is **n** seconds after (or before, if **n** is negative) **d**. Useful for simple offsets.

```first
interaction main() -> Unit {
  let d = now();
  let oneHourLater = addSeconds(d, 3600);
  let oneDayAgo = addSeconds(d, -86400);
  println("Now: " + format(d, "%Y-%m-%d %H:%M:%S"));
  println("One hour later: " + format(oneHourLater, "%H:%M:%S"));
  println("One day ago: " + format(oneDayAgo, "%Y-%m-%d"));
}
```

---

## 6. Putting it together

A small program that shows current time, parses a date, and prints components:

```first
import { now, format, parse, getYear, getMonth, getDay, addSeconds } "Date"
import "Prelude"

interaction main() -> Unit {
  println("=== Current time ===");
  let d = now();
  println(format(d, "%Y-%m-%d %H:%M:%S"));
  println("Year: " + intToString(getYear(d)) + ", Month: " + intToString(getMonth(d)) + ", Day: " + intToString(getDay(d)));

  println("");
  println("=== Parsed date ===");
  let holiday = parse("2025-07-04");
  println(format(holiday, "%Y-%m-%d"));

  println("");
  println("=== One day later ===");
  let nextDay = addSeconds(holiday, 86400);
  println(format(nextDay, "%Y-%m-%d"));
}
```

You can run the full example from the **chapter-11-Date-functions** example project (see README).
