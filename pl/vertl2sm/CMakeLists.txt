project(vertl2sm)
cmake_minimum_required(VERSION 2.8)
SET(CMAKE_BUILD_TYPE debug)
set(CMAKE_CXX_FLAGS -std=c++11)
set(SRC_LIST main.cpp astnode.cpp parser.cpp visitorsm.cpp global.h astnode_fwd.h)
add_executable(${PROJECT_NAME} ${SRC_LIST})

