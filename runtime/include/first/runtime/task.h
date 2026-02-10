#pragma once

#ifdef __cplusplus
extern "C" {
#endif

// Task (spawn/join) and promise (async/await) runtime.
// first_task_spawn(fn): fn is an opaque pointer to a function void* (void). Run fn() in a new thread; returns handle.
// first_task_join(handle): block until task completes, return result (void*), then free handle.

void* first_task_spawn(void* fn);
void* first_task_join(void* handle);

#ifdef __cplusplus
}
#endif
