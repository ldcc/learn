project(learn)
set(CMAKE_C_FLAGS_INIT "-Wall -std=c20")
set(CMAKE_C_COMPILER "/usr/bin/clang-10")

add_executable(learn nous/alignment.c)