
cmake_minimum_required(VERSION 3.0)

project(lspserver)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)

set(PACLET "LSPServer")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(BUILD_DOCS OFF CACHE BOOL "Build documentation")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")

message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")

set(WL_PACLET_SOURCES
	${CMAKE_SOURCE_DIR}/LSPServer/Kernel/Color.wl
	${CMAKE_SOURCE_DIR}/LSPServer/Kernel/Hover.wl
	${CMAKE_SOURCE_DIR}/LSPServer/Kernel/Library.wl
	${CMAKE_SOURCE_DIR}/LSPServer/Kernel/LSPServer.wl
	${CMAKE_SOURCE_DIR}/LSPServer/Kernel/Utils.wl
)

set(WL_GENERATE_CREATEPACLETARCHIVE_SOURCES
	${CMAKE_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
)

set(PACLETINFO_SOURCE
	${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m
)

set(DOCUMENTATION_NOTEBOOK_PATHS
	#English/ReferencePages/Symbols/StartServer.nb
)

#
# Setup documentation notebook paths
#
foreach(NB_PATH ${DOCUMENTATION_NOTEBOOK_PATHS})
	set(SRC ${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/${NB_PATH})
	set(BUILT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/${NB_PATH})
	list(APPEND DOCUMENTATION_SOURCE ${SRC})
	list(APPEND BUILT_DOCUMENTATION ${BUILT})
endforeach()



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()


#
# Version 12 introduced NumericArrays in LibraryLink
#
if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1200)
	message(FATAL_ERROR "Wolfram Kernel must be at least version 12.0: ${VERSION_NUMBER}")
endif()


file(MAKE_DIRECTORY
	${CMAKE_BINARY_DIR}/paclet/${PACLET}
)


#
# Copy WL source files
#

set(COPIED_PACLETINFO ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

add_custom_command(
	OUTPUT
		${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m ${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DLOCAL_BUILD=${LOCAL_BUILD} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m
)
list(APPEND COPIED_WL_PACLET_SOURCES ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

foreach(SRC ${WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    DEPENDS ${SRC})
	list(APPEND COPIED_WL_PACLET_SOURCES ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()







#
# Build LSPServer library
#

set(WOLFRAMLIBRARY_INCLUDE_DIR ${WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

set(CPP_INCLUDES
	${CMAKE_SOURCE_DIR}/cpp/include/LSPServer.h
)

set(CPP_SOURCES
	${CMAKE_SOURCE_DIR}/cpp/src/LSPServer.cpp
)

add_library(lspserver-lib SHARED
	${CPP_INCLUDES}
	${CPP_SOURCES}
)

target_include_directories(lspserver-lib
	PRIVATE ${CMAKE_SOURCE_DIR}/cpp/include
	PRIVATE ${WOLFRAMLIBRARY_INCLUDE_DIR}
)


set_target_properties(lspserver-lib PROPERTIES
	OUTPUT_NAME
		LSPServer
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#library-output-artifacts
	#
	LIBRARY_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	#
	LIBRARY_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#archive-output-artifacts
	#
	ARCHIVE_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#runtime-output-artifacts
	#
	RUNTIME_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# WL / LibraryLink style dictates that the library be named "LSPServer.dylib" and not "libLSPServer.dylib"
	#
	PREFIX
		""
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(lspserver-lib
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else(MSVC)
	target_compile_options(lspserver-lib
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)





set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
	lspserver-lib
	${WL_GENERATE_CREATEPACLETARCHIVE_SOURCES}
)






if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION}
)

endif(BUILD_DOCS)





#
# docs target
#

if(BUILD_DOCS)

add_custom_target(docs
	DEPENDS
		${BUILT_DOCUMENTATION}
)

set(DOCS_APPPATH /Applications/Eclipse.app/Contents/Eclipse/configuration/org.eclipse.osgi/525/0/.cp/MathematicaSource/ CACHE PATH "Path of MathematicaSource inside Workbench installation")

message(STATUS "DOCS_APPPATH: ${DOCS_APPPATH}")

if (NOT IS_DIRECTORY ${DOCS_APPPATH})
message(FATAL_ERROR "DOCS_APPPATH (${DOCS_APPPATH}) does not exist")
endif(NOT IS_DIRECTORY ${DOCS_APPPATH})

set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

message(STATUS "DOCS_JLINKPATH: ${DOCS_JLINKPATH}")

if (NOT IS_DIRECTORY ${DOCS_JLINKPATH})
message(FATAL_ERROR "DOCS_JLINKPATH (${DOCS_JLINKPATH}) does not exist")
endif(NOT IS_DIRECTORY ${DOCS_JLINKPATH})


#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION}
	DEPENDS
		${DOCUMENTATION_SOURCE}
	COMMAND
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/ -DoutputDir=${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)

endif(BUILD_DOCS)






#
# paclet target
#
add_custom_target(paclet
	DEPENDS ${CMAKE_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${CMAKE_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
	DEPENDS
		${PACLET_SOURCES}
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${CMAKE_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl -DSRCDIR=${CMAKE_SOURCE_DIR} -DBUILDDIR=${CMAKE_BINARY_DIR} -DPACLET=${PACLET} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)



