#include "first/runtime/task.h"
#include <thread>
#include <mutex>
#include <condition_variable>
#include <memory>

namespace {

struct TaskState {
    std::mutex m;
    std::condition_variable cv;
    void* result = nullptr;
    bool done = false;
};

void task_thread_fn(void* fn, TaskState* state) {
    void* (*fp)(void) = reinterpret_cast<void* (*)(void)>(fn);
    void* res = fp();
    {
        std::lock_guard<std::mutex> lock(state->m);
        state->result = res;
        state->done = true;
    }
    state->cv.notify_one();
}

}  // namespace

extern "C" {

void* first_task_spawn(void* fn) {
    if (!fn) return nullptr;
    auto* state = new TaskState();
    std::thread t(task_thread_fn, fn, state);
    t.detach();
    return static_cast<void*>(state);
}

void* first_task_join(void* handle) {
    if (!handle) return nullptr;
    TaskState* state = static_cast<TaskState*>(handle);
    std::unique_lock<std::mutex> lock(state->m);
    state->cv.wait(lock, [state] { return state->done; });
    void* result = state->result;
    delete state;
    return result;
}

}  // extern "C"
