#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_main.cpp"

TEST(error_reporter_basic) {
    first::ErrorReporter reporter;
    
    ASSERT(!reporter.hasErrors(), "Should start with no errors");
    ASSERT_EQ(reporter.getErrorCount(), 0, "Error count should be 0");
    
    first::SourceLocation loc(10, 5);
    reporter.error(loc, "Test error");
    
    ASSERT(reporter.hasErrors(), "Should have errors after adding one");
    ASSERT_EQ(reporter.getErrorCount(), 1, "Error count should be 1");
}

TEST(error_reporter_warnings) {
    first::ErrorReporter reporter;
    
    first::SourceLocation loc(5, 3);
    reporter.warning(loc, "Test warning");
    
    ASSERT(!reporter.hasErrors(), "Warnings should not count as errors");
    ASSERT_EQ(reporter.getWarningCount(), 1, "Warning count should be 1");
}

TEST(error_reporter_clear) {
    first::ErrorReporter reporter;
    
    first::SourceLocation loc(1, 1);
    reporter.error(loc, "Error 1");
    reporter.error(loc, "Error 2");
    
    ASSERT_EQ(reporter.getErrorCount(), 2, "Should have 2 errors");
    
    reporter.clear();
    
    ASSERT(!reporter.hasErrors(), "Should have no errors after clear");
    ASSERT_EQ(reporter.getErrorCount(), 0, "Error count should be 0 after clear");
}
