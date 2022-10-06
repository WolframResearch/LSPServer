
#
# https://cmake.org/cmake/help/latest/release/3.15.html
# The cmake(1) command gained a new --install option. This may be used after building a project to run installation without using the generated build system or the native build tool.
#
cmake_minimum_required(VERSION 3.15)

project(lspserver
	LANGUAGES
		CXX
)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)
include(WolframLibrary)

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
set(PACLET_LAYOUT_DIR "paclet" CACHE FILEPATH "Path to complete, built paclet layout (relative to build directory)")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")
#
# Time to pause when starting kernel
#
# Work-around for bug 349779 is to pause ~1 second
# bug 349779 was fixed in version 12.0
# 
# Related bugs: 349779
#
set(KERNEL_PAUSE 0 CACHE STRING "Kernel startup pause")
#
# Timeout for starting a kernel and getting a result
#
# RE machines can be very slow when starting a kernel, so we need to be very generous with this timeout
#
# Should be at least 10 minutes = 600 seconds
#
# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes = 1800 seconds
# bug 349779 was fixed in version 12.0
#
# Related bugs: 349779
# Related issues: RE-514227
#
set(KERNEL_TIMEOUT 600 CACHE STRING "Kernel startup timeout")

if(NOT DEFINED BUILDNUMBER)
set(BUILDNUMBER 0)
endif()
message(STATUS "BUILDNUMBER: ${BUILDNUMBER}")
message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "PACLET_LAYOUT_DIR: ${PACLET_LAYOUT_DIR}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")
if(LOCAL_BUILD)
message(STATUS "Configuring for local build")
endif()
message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "KERNEL_PAUSE: ${KERNEL_PAUSE}")
message(STATUS "KERNEL_TIMEOUT: ${KERNEL_TIMEOUT}")
message(STATUS "CODEPARSER_EXE: ${CODEPARSER_EXE}")
if(NOT CODEPARSER_EXE)
message(STATUS "Optional tool CODEPARSER_EXE was not found; skipping")
endif()

set(WOLFRAMLIBRARY_INCLUDE_DIR ${WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

ParseWolframLibraryHeader()

message(STATUS "WOLFRAMLIBRARY_VERSION: ${WOLFRAMLIBRARY_VERSION}")

if(WOLFRAMLIBRARY_VERSION GREATER_EQUAL 7)
message(WARNING "WOLFRAMLIBRARY_VERSION >= 7 is only compatible with Wolfram System 13.1+ and will not work with earlier versions.")
endif()


set(STATIC_WL_PACLET_KERNEL_SOURCES
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/BracketMismatches.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/CodeAction.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Color.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Definitions.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Diagnostics.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/DocumentSymbol.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/FoldingRange.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Formatting.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Hover.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/ImplicitTokens.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Library.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/ListenSocket.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/LSPServer.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/References.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/SelectionRange.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/SemanticTokens.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/ServerDiagnostics.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Socket.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/StdIO.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Utils.wl
	${PROJECT_SOURCE_DIR}/LSPServer/Kernel/Workspace.wl
)

set(PROCESSED_DATA_FILES
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/AnalyzableMessagePositions.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/AnalyzableMessages.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/BadSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/BuiltinFunctions.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/Constants.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/ExperimentalSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/FreeCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/FreeLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/ObsoleteSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/Options.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SessionSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SpecialCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SpecialLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SystemCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/SystemLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UndocumentedCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UndocumentedLongNames.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UndocumentedSymbols.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UnsupportedCharacters.wl
	${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/UnsupportedLongNames.wl
)

set(PACLETINFO_IN_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.wl.in
)

set(GENERATED_WL_PACLET_KERNEL_SOURCES
	${PROJECT_BINARY_DIR}/generated/wl/Kernel/ReplaceLongNamePUA.wl
)



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()

#
# Force re-configure if PacletInfo.wl.in changes, e.g. paclet version is changed and name of .paclet has changed
#
set_property(
	DIRECTORY
	APPEND
	PROPERTY
		CMAKE_CONFIGURE_DEPENDS
	${PACLETINFO_IN_SOURCE}
)


if(NOT VERSION_NUMBER GREATER_EQUAL 1200)
message(FATAL_ERROR "VERSION_NUMBER is below 1200; LSPServer uses NumericArrays that were added to LibraryLink in version 12.0. (VERSION_NUMBER is ${VERSION_NUMBER})")
endif()

if(NOT VERSION_NUMBER GREATER_EQUAL 1210)
message(WARNING "VERSION_NUMBER is below 1210; ForceVersionInstall was added in 12.1. Installing paclets via CMake may not work. (VERSION_NUMBER is ${VERSION_NUMBER})")
endif()

if(APPLE)
	include(MacOSXVersionMin)
	CheckMacOSXVersionMin()
    if(NOT MACOSX_VERSION_MIN STREQUAL CMAKE_OSX_DEPLOYMENT_TARGET)
    message(WARNING
    	"MACOSX_VERSION_MIN: ${MACOSX_VERSION_MIN}\n"
    	"CMAKE_OSX_DEPLOYMENT_TARGET: ${CMAKE_OSX_DEPLOYMENT_TARGET}\n"
   		"MACOSX_VERSION_MIN and CMAKE_OSX_DEPLOYMENT_TARGET do not match\n"
   		"HINT: Specify -DCMAKE_OSX_DEPLOYMENT_TARGET=${MACOSX_VERSION_MIN} to ensure LSPServer is built with same MacOSXVersionMin as CCompilerDriver libraries.")
    endif()
endif()

file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
	${PROJECT_BINARY_DIR}/generated/wl/Kernel
	${PROJECT_BINARY_DIR}/pdb-output/
)


#
# Copy WL source files
#

set(REPLACED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.wl)

add_custom_command(
	OUTPUT
		${REPLACED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PACLETINFO_IN_SOURCE} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	COMMAND
		${CMAKE_COMMAND} -DTRANSPORT=${TRANSPORT} -DBUILDNUMBER=${BUILDNUMBER} -DVERSION_NUMBER=${VERSION_NUMBER} -DWOLFRAMLIBRARY_VERSION=${WOLFRAMLIBRARY_VERSION} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DPACLETINFO_IN_SOURCE=${PACLETINFO_IN_SOURCE} -DREPLACED_PACLETINFO=${REPLACED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${PACLETINFO_IN_SOURCE}
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)


#
# static Kernel sources
#
foreach(SRC ${STATIC_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		DEPENDS
			${SRC}
			${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL})
endforeach()

#
# processed Data sources
#
foreach(SRC ${PROCESSED_DATA_FILES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/WolframLanguageSyntax/Data/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Data/${REL}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Data/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Data/${REL})
endforeach()

#
# generated Kernel sources
#
foreach(SRC ${GENERATED_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_BINARY_DIR}/generated/wl/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL})
endforeach()



#
# generated srcs
#

#
# ReplaceLongNamePUA files
#
add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/generated/wl/Kernel/ReplaceLongNamePUA.wl
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/ReplaceLongNamePUA.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PROJECT_BINARY_DIR}/generated/wl/Kernel/ReplaceLongNamePUA.wl -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Data/LongNames.wl
		${PROJECT_SOURCE_DIR}/LSPServer/Generate/ReplaceLongNamePUA.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)


#
# Build LSPServer library
#

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
	CXX_VISIBILITY_PRESET
		hidden
	VISIBILITY_INLINES_HIDDEN
		ON
	#
	# prevent
	# /Volumes/TeamCity/work/4466f913884ff7b4/MathLink/CompilerAdditions
	# from being added to rpath
	#
	SKIP_BUILD_RPATH
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
	PDB_OUTPUT_DIRECTORY
		${PROJECT_BINARY_DIR}/pdb-output
	PDB_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/pdb-output
	PDB_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/pdb-output
	#
	# WL / LibraryLink style dictates that the library be named "LSPServer.dylib" and not "libLSPServer.dylib"
	#
	PREFIX
		""
)


if(MSVC)
target_compile_options(lspserver-lib
	#
	# specify /EHsc to force compiler to assume that functions declared as extern "C" never throw a C++ exception
	#
	# specify /MT to statically link runtime on Windows
	# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
	# and fixes bug 427427:
	# error 126 "The specified module could not be found"
	# when Visual Studio is not installed
	PRIVATE /EHsc /MT
)
endif(MSVC)


if(ENABLE_WARNINGS)

#
# Setup warnings
#
# https://www.foonathan.net/2018/10/cmake-warnings/
#
if(MSVC)
	target_compile_options(lspserver-lib
		PRIVATE /W4 /wd4100
	)
else(MSVC)
	target_compile_options(lspserver-lib
		PRIVATE -pedantic-errors -Wall -Wextra -Wconversion -Wsign-conversion -Wno-unused-parameter
	)
endif(MSVC)

endif(ENABLE_WARNINGS)



#
# paclet layout
#

set(PACLET_SOURCES
	${REPLACED_PACLETINFO}
	${COPIED_WL_PACLET_SOURCES}
	lspserver-lib
)



#
# paclet archive
#

if(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${LOCAL_BUILD_VERSION}.paclet)
else(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet)
endif(LOCAL_BUILD)

add_custom_target(create-paclet-archive
	ALL
	DEPENDS
		${PACLET_OUTPUT}
)

#
# CreatePacletArchive
#
add_custom_command(
	OUTPUT
		${PACLET_OUTPUT}
	COMMAND
		#
		# CreatePacletArchive may be slow on RE machines, so allow re-trying if JLink connection timeout is hit
		#
		# see: RE-515885
		#
		${CMAKE_COMMAND} -DRETRY_ON_FAILURE=ON -DSCRIPT=${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET_LAYOUT_DIR=${PACLET_LAYOUT_DIR} -DPACLET=${PACLET} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_BINARY_DIR}
)

install(
	CODE
		"execute_process(COMMAND ${CMAKE_COMMAND} -DPACLET_OUTPUT=${PACLET_OUTPUT} -DPACLET_WOLFRAMVERSION=${PACLET_WOLFRAMVERSION} \"-DWOLFRAMKERNEL=${WOLFRAMKERNEL}\" -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InstallPaclet.cmake)"
	COMPONENT
		paclet
)
