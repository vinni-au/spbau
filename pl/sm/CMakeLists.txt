project(sm)
cmake_minimum_required(VERSION 2.8)
SET(CMAKE_BUILD_TYPE release)
aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})

