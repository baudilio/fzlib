# Top-level CMakeLists.txt for example
###
### Process this file with cmake to produce Makefile
###

include_directories(${CMAKE_BINARY_DIR}/lib)

add_executable(scanfs.x main.f90)

target_link_libraries(scanfs.x lib -lz)
