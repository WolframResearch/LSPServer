
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


EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT int ReadLineFromStdIn_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int ReadBytesFromStdIn_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int WriteLineToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

EXTERN_C DLLEXPORT int WriteBytesToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);





