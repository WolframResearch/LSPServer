
cmake_minimum_required(VERSION 3.0)

project(lspserver
	LANGUAGES
		CXX
)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)

#
# Used for quickly reporting syntax errors in WL source files
#
find_program(CODEPARSER_EXE
	NAMES
		codeparser codeparser.exe
	HINTS
		${CODEPARSER_EXE_DIR}
)

set(PACLET "LSPServer")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")
# Work-around for bug 349779 is to pause ~1 second
set(BUG349779_PAUSE 1 CACHE STRING "Bug 349779 pause")
#
# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes
# So double that and cross fingers.
#
# Related bugs: 349779
# Related issues: RE-514227
#
set(BUG349779_TIMEOUT 3600 CACHE STRING "Bug 349779 timeout")

message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")
message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "BUG349779_PAUSE: ${BUG349779_PAUSE}")
message(STATUS "BUG349779_TIMEOUT: ${BUG349779_TIMEOUT}")
message(STATUS "CODEPARSER_EXE: ${CODEPARSER_EXE}")

set(STATIC_WL_PACLET_SOURCES
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/BracketMismatches.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/CodeAction.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Color.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Definitions.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Diagnostics.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/DocumentSymbol.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Formatting.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Hover.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/ImplicitTokens.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Library.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/LSPServer.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/References.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/ServerDiagnostics.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Utils.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Workspace.wl
)

#
# Used during the build process
#
set(WL_GENERATE_SOURCES_SOURCES
	${PROJECT_SOURCE_DIR}/LSPServer/Generate/GenerateSources.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Generate/ReplacePUA.wl
)

set(PROCESSED_SYMBOLS_DUMP ${PROJECT_BINARY_DIR}/processedSymbols.mx)

set(WL_GENERATE_CREATEPACLETARCHIVE_SOURCES
	${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
)

set(PACLETINFO_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.wl.in
)

set(WL_DATA_SOURCES
	${PROJECT_SOURCE_DIR}/LSPServer/Data/LongNames.wl
)

set(GENERATED_WL_PACLET_KERNEL_SOURCES
	${PROJECT_BINARY_DIR}/generated/wl/ReplacePUA.wl
)

set(GENERATED_SOURCES
	${GENERATED_WL_PACLET_KERNEL_SOURCES}
)



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
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
)


#
# Copy WL source files
#

set(COPIED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.wl)

add_custom_command(
	OUTPUT
		${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PACLETINFO_SOURCE} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	COMMAND
		${CMAKE_COMMAND} -E copy ${PACLETINFO_SOURCE} ${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${PACLETINFO_SOURCE}
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)
list(APPEND COPIED_WL_PACLET_SOURCES ${COPIED_PACLETINFO})

foreach(SRC ${STATIC_WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		DEPENDS
			${SRC}
			${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()

foreach(SRC ${GENERATED_WL_PACLET_KERNEL_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		DEPENDS
			${SRC}
			${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()



#
# generated-srcs target
#
add_custom_target(generated-srcs
	DEPENDS
		${GENERATED_SOURCES}
)

#
# Generate source files
#
# Run the GenerateSources.wl script to generate additional required source files
#
add_custom_command(
	OUTPUT
		${GENERATED_SOURCES}
	DEPENDS
		${WL_DATA_SOURCES}
		${WL_GENERATE_SOURCES_SOURCES}
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -E echo "GENERATED_WL_PACLET_SOURCES: ${GENERATED_WL_PACLET_SOURCES}"
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/GenerateSources.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)


add_custom_command(
	OUTPUT
		${PROCESSED_SYMBOLS_DUMP}
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/LSPServer/Generate/ProcessSymbols.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/LSPServer/Generate/ProcessSymbols.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
)

set(COPIED_PROCESSED_SYMBOLS_DUMP ${PROJECT_BINARY_DIR}/paclet/LSPServer/Resources/Generated/processedSymbols.mx)

add_custom_command(
	OUTPUT
		${COPIED_PROCESSED_SYMBOLS_DUMP}
	COMMAND
		${CMAKE_COMMAND} -E copy ${PROCESSED_SYMBOLS_DUMP} ${COPIED_PROCESSED_SYMBOLS_DUMP}
	DEPENDS
		${PROCESSED_SYMBOLS_DUMP}
)





#
# Build LSPServer library
#

set(WOLFRAMLIBRARY_INCLUDE_DIR ${WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

set(CPP_INCLUDES
	${PROJECT_SOURCE_DIR}/cpp/include/LSPServer.h
)

set(CPP_SOURCES
	${PROJECT_SOURCE_DIR}/cpp/src/LSPServer.cpp
)

add_library(lspserver-lib SHARED
	${CPP_INCLUDES}
	${CPP_SOURCES}
)

target_include_directories(lspserver-lib
	PRIVATE ${PROJECT_SOURCE_DIR}/cpp/include
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
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	#
	LIBRARY_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#archive-output-artifacts
	#
	ARCHIVE_OUTPUT_DIRECTORY
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#runtime-output-artifacts
	#
	RUNTIME_OUTPUT_DIRECTORY
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
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
	${COPIED_PROCESSED_SYMBOLS_DUMP}
	lspserver-lib
	${WL_GENERATE_CREATEPACLETARCHIVE_SOURCES}
)






#
# paclet target
#

if(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${LOCAL_BUILD_VERSION}.paclet)
else(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet)
endif(LOCAL_BUILD)

add_custom_target(paclet
	DEPENDS
		${PACLET_OUTPUT}
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${PACLET_OUTPUT}
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET=${PACLET} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)



