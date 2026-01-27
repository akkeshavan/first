#include "first/runtime/refcount.h"
#include <cassert>
#include <iostream>

using namespace first::runtime;

// Test class for reference counting
class TestObject : public RefCounted {
public:
    TestObject(int value) : value_(value) {}
    int getValue() const { return value_; }
    void setValue(int v) { value_ = v; }
    
    static int destructorCount;
    ~TestObject() {
        destructorCount++;
    }

private:
    int value_;
};

int TestObject::destructorCount = 0;

void test_basic_refcount() {
    TestObject::destructorCount = 0;
    
    {
        Ref<TestObject> ref1 = makeRef<TestObject>(42);
        assert(ref1->getValue() == 42);
        // Object created with count=0, Ref constructor increments to 1
        assert(ref1->getRefCount() == 1);
        
        {
            Ref<TestObject> ref2 = ref1; // Copy increments to 2
            assert(ref1->getRefCount() == 2);
            assert(ref2->getRefCount() == 2);
            assert(ref2->getValue() == 42);
        } // ref2 destroyed, decrements to 1
        
        assert(ref1->getRefCount() == 1);
    } // ref1 destroyed, decrements to 0 and deletes
    
    // Object should be deleted exactly once
    assert(TestObject::destructorCount == 1);
    std::cout << "✓ Basic reference counting works\n";
}

void test_move_semantics() {
    TestObject::destructorCount = 0;
    
    {
        Ref<TestObject> ref1 = makeRef<TestObject>(100);
        Ref<TestObject> ref2 = std::move(ref1);
        
        assert(ref1.get() == nullptr);
        assert(ref2->getValue() == 100);
        assert(ref2->getRefCount() == 1);
    }
    
    assert(TestObject::destructorCount == 1);
    std::cout << "✓ Move semantics work\n";
}

void test_assignment() {
    TestObject::destructorCount = 0;
    
    {
        Ref<TestObject> ref1 = makeRef<TestObject>(10);
        Ref<TestObject> ref2 = makeRef<TestObject>(20);
        
        ref1 = ref2; // Assignment
        
        assert(ref1->getValue() == 20);
        assert(ref1->getRefCount() == 2);
        assert(ref2->getRefCount() == 2);
    }
    
    assert(TestObject::destructorCount == 2);
    std::cout << "✓ Assignment works\n";
}

void test_null_ref() {
    Ref<TestObject> ref;
    assert(ref.get() == nullptr);
    assert(!ref);
    std::cout << "✓ Null reference works\n";
}

int main() {
    std::cout << "Testing Reference Counting Runtime...\n\n";
    
    test_basic_refcount();
    test_move_semantics();
    test_assignment();
    test_null_ref();
    
    std::cout << "\nAll reference counting tests passed!\n";
    return 0;
}
