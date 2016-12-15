cmake_minimum_required ( VERSION 2.8.11 )
enable_language (Fortran)

project ( gmxfort NONE )

set ( PROJECT_DESCRIPTION "A Fortran 2008 JSON API" )
set ( PROJECT_URL "https://github.com/wesbarnett/libgmxfort" )

find_library ( LIBXDRFILE xdrfile HINTS ENV LD_LIBRARY_PATH REQUIRED )
message(STATUS "Found xdrfile library at: ${LIBXDRFILE}")
find_path(
   XDRFILE_INCLUDES
   NAMES "xdrfile/xdrfile.h"
   NAMES "xdrfile/xdrfile_xtc.h"
   NAMES "xdrfile.h"
   NAMES "xdrfile_xtc.h"
   HINTS ENV CPLUS_INCLUDE_PATH
)
message(STATUS "Found xdrfile headers at: ${XDRFILE_INCLUDES}")

file ( GLOB SRCS "src/*.f90" )
add_library ( ${CMAKE_PROJECT_NAME} SHARED ${SRCS} )
target_link_libraries ( ${CMAKE_PROJECT_NAME} ${LIBXDRFILE} )
target_include_directories ( ${CMAKE_PROJECT_NAME} PUBLIC ${XDRFILE_INCLUDES} )

set_target_properties ( ${CMAKE_PROJECT_NAME}
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
  PREFIX lib
# SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
# VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include" )

install ( TARGETS gmxfort LIBRARY DESTINATION lib )

enable_testing ()
include_directories ( include )
add_executable ( dotest src/tests/test.f90 )
target_link_libraries ( dotest gmxfort )
add_test ( do_test dotest )
