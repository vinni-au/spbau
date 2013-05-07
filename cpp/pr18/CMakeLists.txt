project(pr18)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)
set(CMAKE_CXX_FLAGS -g3)
add_executable(${PROJECT_NAME} ${SRC_LIST})

