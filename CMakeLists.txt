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
file ( GLOB SRCS "src/*.f90" )
add_library ( ${CMAKE_PROJECT_NAME} SHARED ${SRCS} )
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
file ( GLOB TEST_SRC "src/tests/gmxfort_tests.f90" )
include_directories( ${CMAKE_CURRENT_BINARY_DIR}/include )
add_library ( gmxfort_tests SHARED ${TEST_SRC} )
target_link_libraries ( gmxfort_tests ${CMAKE_PROJECT_NAME} )

add_executable ( xtcfile src/tests/xtcfile.f90 )
target_link_libraries ( xtcfile gmxfort_tests )
add_test ( "Read_xtcfile" xtcfile )

add_executable ( ndxfile src/tests/ndxfile.f90 )
target_link_libraries ( ndxfile gmxfort_tests )
add_test ( "Read_xtcfile_and_ndxfile" ndxfile )

add_executable ( ndxfile_grp src/tests/ndxfile_grp.f90 )
target_link_libraries ( ndxfile_grp gmxfort_tests )
add_test ( "Read_ndx_grp" ndxfile_grp )

add_executable ( read_next src/tests/read_next.f90 )
target_link_libraries ( read_next gmxfort_tests )
add_test ( "Read_next" read_next )

add_executable ( utils src/tests/utils.f90 )
target_link_libraries ( utils gmxfort_tests )
add_test ( "Utils" utils )

configure_file(src/tests/test.xtc ${CMAKE_CURRENT_BINARY_DIR}/test.xtc COPYONLY)
configure_file(src/tests/test.ndx ${CMAKE_CURRENT_BINARY_DIR}/test.ndx COPYONLY)
