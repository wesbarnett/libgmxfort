#
# This file is part of libgmxfort
# https://github.com/wesbarnett/libgmxfort
#
# Copyright (c) 2016,2017 by James W. Barnett
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

cmake_minimum_required ( VERSION 2.8.8 )
enable_language (Fortran)

project ( gmxfort NONE )

set ( PROJECT_DESCRIPTION "Fortran toolkit for analyzing GROMACS simulations" )
set ( PROJECT_URL "https://github.com/wesbarnett/libgmxfort" )
set ( LIBDIR "lib")
set ( INCLUDEDIR "include")
set ( INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/${LIBDIR} )
set ( INSTALL_INCLUDEDIR ${CMAKE_INSTALL_PREFIX}/${INCLUDEDIR} )

# -------------------------
# Package version
# -------------------------
file ( STRINGS "${CMAKE_SOURCE_DIR}/.VERSION" VERSION )
string( REPLACE "." ";" VERSION_LIST ${VERSION} )
list(GET VERSION_LIST 0 VERSION_MAJOR)
list(GET VERSION_LIST 1 VERSION_MINOR)
list(GET VERSION_LIST 2 VERSION_PATCH)
set(PROJECT_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
message ( STATUS "CMake build configuration for ${CMAKE_PROJECT_NAME} ${PROJECT_VERSION}" )
string ( TOLOWER ${CMAKE_PROJECT_NAME} PACKAGE_NAME )
set ( PACKAGE_VERSION "${PACKAGE_NAME}-${VERSION}" )

# -------------------------
# Find xdrfile
# -------------------------
find_package ( xdrfile REQUIRED )
message(STATUS "Found xdrfile library at: ${xdrfile_LIBRARIES}")
message(STATUS "Found xdrfile include dirs at: ${xdrfile_INCLUDE_DIRS}")

# -------------------------
# Create library
# -------------------------
add_library ( ${CMAKE_PROJECT_NAME} SHARED src/common.f90 src/indexfile.f90 src/trajectory.f90 src/utils.f90 )
target_link_libraries ( ${CMAKE_PROJECT_NAME} ${xdrfile_LIBRARIES} )
target_include_directories ( ${CMAKE_PROJECT_NAME} PUBLIC ${xdrfile_INCLUDE_DIRS} )

set_target_properties ( ${CMAKE_PROJECT_NAME}
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include" )

install ( TARGETS ${CMAKE_PROJECT_NAME} LIBRARY DESTINATION lib )
install ( DIRECTORY "${CMAKE_BINARY_DIR}/include" DESTINATION "${CMAKE_INSTALL_PREFIX}" )

#-----------------------------------------------------
# Publicize installed location to other CMake projects
#-----------------------------------------------------
set ( EXPORT_INSTALL_DIR "${INSTALL_LIBDIR}/cmake/${PACKAGE_VERSION}" )

include ( CMakePackageConfigHelpers ) # Standard CMake module
write_basic_package_version_file(
"${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  VERSION ${VERSION}
  COMPATIBILITY SameMajorVersion )

# install package config file
configure_package_config_file (
  "${CMAKE_SOURCE_DIR}/cmake/pkg/${CMAKE_PROJECT_NAME}-config.cmake.in"
  "${CMAKE_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  INSTALL_DESTINATION "${EXPORT_INSTALL_DIR}"
  PATH_VARS EXPORT_INSTALL_DIR)

# Install the config and version files so that we can find this project with
# others
install ( FILES
  "${CMAKE_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  DESTINATION "${EXPORT_INSTALL_DIR}" )

# pkg-config
configure_file(
   "${CMAKE_CURRENT_SOURCE_DIR}/cmake/lib${CMAKE_PROJECT_NAME}.pc.cmake.in"
   "${CMAKE_CURRENT_BINARY_DIR}/lib${CMAKE_PROJECT_NAME}.pc"
   @ONLY
)
install ( FILES
   "${CMAKE_CURRENT_BINARY_DIR}/lib${CMAKE_PROJECT_NAME}.pc"
   DESTINATION "${LIBDIR}/pkgconfig"
)

#-----------------------------------------------------
# Tests
#-----------------------------------------------------
enable_testing ()
set ( TEST_SRC "src/tests/gmxfort_tests.f90" )
include_directories( ${CMAKE_CURRENT_BINARY_DIR}/include )
add_library ( gmxfort_tests SHARED ${TEST_SRC} )
target_link_libraries ( gmxfort_tests ${CMAKE_PROJECT_NAME} )

foreach(TESTNAME xtcfile ndxfile ndxfile_grp read_next utils)
    add_executable ( ${TESTNAME} src/tests/${TESTNAME}.f90 )
    target_link_libraries ( ${TESTNAME} gmxfort_tests )
endforeach(TESTNAME)

add_test ( "Read_xtcfile" xtcfile )
add_test ( "Read_xtcfile_and_ndxfile" ndxfile )
add_test ( "Read_ndx_grp" ndxfile_grp )
add_test ( "Read_next" read_next )
add_test ( "Utils" utils )

configure_file(src/tests/test.xtc ${CMAKE_CURRENT_BINARY_DIR}/test.xtc COPYONLY)
configure_file(src/tests/test.ndx ${CMAKE_CURRENT_BINARY_DIR}/test.ndx COPYONLY)
