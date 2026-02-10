# Chapter 17: Concurrency and async model

First provides **concurrency and asynchronous** constructs so you can run work in the background and wait for results without blocking. This chapter describes the **async/await** model (promises), the **spawn/join** model (tasks), and the **select** construct for channel-based concurrency. **Spawn/join** and **async/await** are implemented: the runtime runs the spawned or async computation in a separate thread and **join** / **await** block until the result is ready. **Select** on channels is parsed and type-checked; channel creation and full select semantics are planned for future releases.

---

## 1. Overview

First supports two main concurrency styles:

| Style        | Constructs     | Purpose |
|-------------|-----------------|--------|
| **Async/await** | `async`, `await` | Turn a computation into a **Promise&lt;T&gt;** and later get the value of type **T** by awaiting it. |
| **Tasks**       | `spawn`, `join`  | Run a computation in a separate **task**; **spawn** returns **Task&lt;T&gt;**; **join** blocks until the task finishes and yields **T**. |
| **Select**      | `select { ... }` | Wait on multiple **channel** operations (receive or send); the first one that becomes ready runs. |

All of these are intended for use in **interactions** (side-effecting code), where I/O and concurrency make sense.

---

## 2. Async and await

### 2.1 async

The **async** keyword turns an expression into an asynchronous computation. Conceptually it produces a **Promise&lt;T&gt;** (or similar) that will eventually yield a value of type **T**.

**Syntax:** `async primaryExpr`

The operand must be a **primary expression** (e.g. a variable, a parenthesized expression, or a call in parentheses). To async-over a function or interaction call, use parentheses:

```first
// async (call()) — parentheses make the call a primary expression
let p = async (fetchData());
```

- **Type:** If the operand has type **T**, the **async** expression has type **Promise&lt;T&gt;** (in the full model). The current type checker may simplify this to **T** until the full promise type is wired through.

### 2.2 await

The **await** keyword takes a promise-like value and yields the result of type **T**. It is used to “unwrap” the result of an **async** computation.

**Syntax:** `await primaryExpr`

**Example:**

```first
interaction fetchData() -> Int {
    return 42;
}

interaction main() -> Unit {
    let promise = async (fetchData());
    let value = await promise;
    println("Got: " + intToString(value));
}
```

- **Type:** If the operand has type **Promise&lt;T&gt;** (or is currently treated as **T**), the **await** expression has type **T**.

---

## 3. Spawn and join (tasks)

### 3.1 spawn

**spawn** starts a computation in a separate **task** and returns a handle of type **Task&lt;T&gt;** (conceptually). The computation runs concurrently; the current flow continues until you **join** the task.

**Syntax:** `spawn primaryExpr`

Again, use parentheses to spawn a call:

```first
let task = spawn (heavyWork());
```

- **Type:** If the operand has type **T**, the **spawn** expression has type **Task&lt;T&gt;** (in the full model; the type checker may currently use **T**).

### 3.2 join

**join** takes a task handle and waits for that task to finish. The result of the **join** expression is the return value of the task, of type **T**.

**Syntax:** `join primaryExpr`

**Example:**

```first
interaction heavyWork() -> Int {
    return 7;
}

interaction main() -> Unit {
    let t = spawn (heavyWork());
    let result = join t;
    println("Task result: " + intToString(result));
}
```

- **Type:** If the operand has type **Task&lt;T&gt;** (or currently **T**), the **join** expression has type **T**.

---

## 4. Select (channels)

The **select** construct lets you wait on multiple **channel** operations. Whichever operation becomes ready first runs its branch.

**Syntax:**

```first
select {
    <- channelExpr => varName: statement   // receive: bind value to varName, then run statement
    channelExpr <- valueExpr: statement    // send: send valueExpr on channel, then run statement
    else: statement                        // default: run if no channel is ready
}
```

- **Receive:** `<- channelExpr => varName: statement` — wait to receive a value from the channel, bind it to **varName**, then execute **statement**.
- **Send:** `channelExpr <- valueExpr: statement` — wait until the channel can accept a value, send **valueExpr**, then execute **statement**.
- **Default:** `else: statement` — run **statement** if no other branch is ready (e.g. to avoid blocking).

Channel types use **Channel&lt;T&gt;** (see the language specification). Creating channels and the full semantics of **select** are part of the planned concurrency runtime; the compiler currently parses and type-checks **select**, but the runtime may stub it (e.g. run the first branch or the **else** branch).

**Example (syntax only; channels may be stubbed):**

```first
select {
    else: println("No channel ready (stub)");
}
```

---

## 5. Full example: spawn/join and async/await

The following interaction uses both **spawn/join** and **async/await** to illustrate the syntax and types. With the current stub semantics, both the task and the “async” call run to completion and you see the printed results.

```first
// Simulated “slow” work (in a real runtime this would run in another task)
interaction slowWork() -> Int {
    return 21;
}

// Simulated “async” fetch (in a real runtime this would be a promise)
interaction fetchNumber() -> Int {
    return 100;
}

interaction main() -> Unit {
    // --- Spawn/join: run work in a task and wait for the result ---
    let task = spawn (slowWork());
    let taskResult = join task;
    println("Spawn/join result: " + intToString(taskResult));

    // --- Async/await: treat a call as async and await its result ---
    let promise = async (fetchNumber());
    let asyncResult = await promise;
    println("Async/await result: " + intToString(asyncResult));
}
```

**Run:** From the repo root, run:

```bash
cd examples/chapter-17-concurrency-async && fir run
```

You should see both lines printed. **spawn** runs **slowWork()** in a separate thread and **join** blocks until it finishes; **async** starts the work in a thread and **await** blocks until the result is ready.

---

## 6. Summary

| Construct | Syntax | Purpose |
|-----------|--------|--------|
| **async** | `async primaryExpr` | Build a promise from a computation (type **Promise&lt;T&gt;**). |
| **await** | `await primaryExpr` | Get the result of a promise (type **T**). |
| **spawn** | `spawn primaryExpr` | Start a task (type **Task&lt;T&gt;**). |
| **join** | `join primaryExpr` | Wait for a task and get its result (type **T**). |
| **select** | `select { branches }` | Wait on channel receive/send or run **else** branch. |

Use **async/await** for promise-style asynchronous flow and **spawn/join** for explicit tasks. Use **select** when you have multiple channels and want to react to the first one that becomes ready. The **chapter-17-concurrency-async** example project contains the full runnable example above.
