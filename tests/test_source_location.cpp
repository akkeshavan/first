#include "first/source_location.h"
#include "test_framework.h"

TEST(source_location_basic) {
    first::SourceLocation loc(10, 20, "test.first");
    
    ASSERT_EQ(loc.getLine(), 10, "Line should be 10");
    ASSERT_EQ(loc.getColumn(), 20, "Column should be 20");
    ASSERT_EQ(loc.getFile(), "test.first", "File should be test.first");
}

TEST(source_location_default) {
    first::SourceLocation loc;
    
    ASSERT_EQ(loc.getLine(), 1, "Default line should be 1");
    ASSERT_EQ(loc.getColumn(), 1, "Default column should be 1");
    ASSERT_EQ(loc.getFile(), "", "Default file should be empty");
}

TEST(source_location_to_string) {
    first::SourceLocation loc(5, 10, "example.first");
    std::string str = loc.toString();
    
    ASSERT_NE(str.find("example.first"), std::string::npos, "Should contain filename");
    ASSERT_NE(str.find("5"), std::string::npos, "Should contain line number");
    ASSERT_NE(str.find("10"), std::string::npos, "Should contain column number");
}
