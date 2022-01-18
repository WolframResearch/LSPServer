
#pragma once

//
// Despite being mentioned here:
// language/LibraryLink/tutorial/LibraryStructure.html
//
// It is not actually possible to include "wstp.h" for use with WolframLibrary.h
//
// Using wstp.h results in errors like:
// error: unknown type name 'MLINK'
//
// Related bugs: 357133
//
// This bug was fixed in v12.0
//
// When we no longer support any version < 12.0, then we can switch to using WSTP
//
// Also be a good citizen and cleanup the leftover defines from mathlink and WolframLibrary
//
#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include "WolframLibrary.h"
#undef True
#undef False

//
// CMake defines codeparser_lib_EXPORTS
//
#ifdef _WIN32
# ifdef lspserver_lib_EXPORTS
#   define LSPSERVERLIB_EXPORTED  __declspec( dllexport )
# else
#   define LSPSERVERLIB_EXPORTED  __declspec( dllimport )
# endif
#else
# define LSPSERVERLIB_EXPORTED
#endif


constexpr int FILENO_ERROR = 1;
constexpr int FILENO_NOT_STREAM = 2;
constexpr int SETMODE_ERROR = 3;

constexpr int FREAD_FAILED = 1;
constexpr int UNEXPECTED_LINEFEED = 2;
constexpr int EXPECTED_LINEFEED = 3;
constexpr int UNRECOGNIZED_HEADER = 4;
constexpr int FWRITE_FAILED = 5;
constexpr int FFLUSH_FAILED = 6;


constexpr int DEBUG_VERBOSE = 3;


EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT int SetDebug_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetStartupError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int ReadLineFromStdIn_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int WriteLineToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int WriteBytesToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);


EXTERN_C DLLEXPORT int StartBackgroundReaderThread_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int LockQueue_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int UnlockQueue_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetQueueSize_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetFrontMessageSize_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int PopQueue_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetBackgroundReaderThreadError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetStdInFEOF_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetStdInFError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetStdOutFEOF_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int GetStdOutFError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
