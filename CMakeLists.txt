# Top-level CMakeLists.txt for example
###
### Process this file with cmake to produce Makefile
###

cmake_minimum_required(VERSION 2.8)

project(example Fortran)

set(PACKAGE Zlib)

#
set(CMAKE_Fortran_COMPILER "gfortran")
set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} "-Wall -O2")

# List of subdirectories to consider
add_subdirectory(prog)
add_subdirectory(lib)
