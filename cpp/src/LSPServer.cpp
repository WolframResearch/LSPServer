
#include "LSPServer.h"

#include "WolframNumericArrayLibrary.h"

#include <string>
#include <cstdio>
#include <cstring>


DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {

}

//
// Lines always end with \r\n on all platforms, so just do simple loop here
//
DLLEXPORT int ReadLineFromStdIn_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    static std::string str;

    str.clear();

    char c;
    while (true) {

        auto r = fread(&c, sizeof(char), 1, stdin);
        if (r != 1) {
            return LIBRARY_FUNCTION_ERROR;
        }

        if (c == '\r') {
            break;
        }
        //
        // EOF can insert \n at the end
        //
        if (c == '\n') {
            return LIBRARY_FUNCTION_ERROR;
        }
        str += c;
    }

    auto r = fread(&c, sizeof(char), 1, stdin);
    if (r != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }

    if (c != '\n') {
        return LIBRARY_FUNCTION_ERROR;
    }

    MArgument_setUTF8String(Res, const_cast<char *>(str.c_str()));

    return LIBRARY_NO_ERROR;
}

DLLEXPORT int ReadBytesFromStdIn_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    MNumericArray na;
    na = MArgument_getMNumericArray(Args[0]);

    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(na);

    auto data = reinterpret_cast<unsigned char *>(libData->numericarrayLibraryFunctions->MNumericArray_getData(na));

    auto r = fread(data, sizeof(unsigned char), numBytes, stdin);
    if (r != numBytes) {
        
        libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

        return LIBRARY_FUNCTION_ERROR;
    }

    libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

    return LIBRARY_NO_ERROR;
}






DLLEXPORT int WriteLineToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    static char newline[] = { '\r', '\n' };

    auto line = MArgument_getUTF8String(Args[0]);

    size_t lineLen = strlen(line);

    auto w = fwrite(line, sizeof(char), lineLen, stdout);
    if (w != lineLen) {

        libData->UTF8String_disown(line);

        return LIBRARY_FUNCTION_ERROR;
    }

    w = fwrite(newline, sizeof(char), 2, stdout);
    if (w != 2) {

        libData->UTF8String_disown(line);

        return LIBRARY_FUNCTION_ERROR;
    }

    if (fflush(stdout)) {

        libData->UTF8String_disown(line);

        return LIBRARY_FUNCTION_ERROR;
    }

    libData->UTF8String_disown(line);

    return LIBRARY_NO_ERROR;
}





DLLEXPORT int WriteBytesToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    MNumericArray na;
    na = MArgument_getMNumericArray(Args[0]);

    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(na);

    auto data = reinterpret_cast<unsigned char *>(libData->numericarrayLibraryFunctions->MNumericArray_getData(na));

    auto w = fwrite(data, sizeof(unsigned char), numBytes, stdout);
    if (w != numBytes) {

        libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

        return LIBRARY_FUNCTION_ERROR;
    }

    if (fflush(stdout)) {

        libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

        return LIBRARY_FUNCTION_ERROR;
    }

    libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

    return LIBRARY_NO_ERROR;
}





