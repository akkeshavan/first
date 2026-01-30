# Ensure dist/ exists before CPack writes packages (CPack does not create the output prefix).
# Use path baked by main CMakeLists when available; else derive from script location.
if(DEFINED CPACK_OUTPUT_FILE_PREFIX AND NOT CPACK_OUTPUT_FILE_PREFIX STREQUAL "")
  set(DIST_DIR "${CPACK_OUTPUT_FILE_PREFIX}")
elseif(DEFINED CMAKE_SCRIPT_LIST_FILE AND NOT CMAKE_SCRIPT_LIST_FILE STREQUAL "")
  get_filename_component(SCRIPT_DIR "${CMAKE_SCRIPT_LIST_FILE}" DIRECTORY)
  get_filename_component(PROJECT_DIR "${SCRIPT_DIR}" DIRECTORY)
  if(NOT PROJECT_DIR STREQUAL "")
    set(DIST_DIR "${PROJECT_DIR}/dist")
  endif()
endif()
if(DEFINED DIST_DIR AND NOT DIST_DIR STREQUAL "")
  file(MAKE_DIRECTORY "${DIST_DIR}")
endif()
