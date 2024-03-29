
#include "LSPServer.h"

#include "WolframNumericArrayLibrary.h"

#include <string>
#include <cstdio>
#include <cstring>
#ifdef _WIN32
#include <io.h> // for _setmode
#include <fcntl.h> // for _O_BINARY
#endif // _WIN32
#include <memory>
#include <queue>
#include <string>
#include <vector>
#include <thread>
#include <regex>
#include <mutex>
#include <algorithm>
#include <cassert>


int readLineFromStdIn(std::string& str);
int readBytesFromStdIn(unsigned char *data, size_t numBytes);
void threadBody();


struct Message {
    std::vector<std::string> Headers;
    std::unique_ptr<unsigned char []> Body;
    size_t Size;
    
    Message() : Headers(), Body(), Size() {}
    Message(std::vector<std::string> Headers, std::unique_ptr<unsigned char []> Body, size_t Size) :
        Headers(Headers), Body(std::move(Body)), Size(Size) {}
};


int debugLevel;

int startupError;

std::thread readerThread;

std::queue<Message> q;
std::mutex qMutex;

std::regex ContentLengthRegEx("Content-Length: (\\d+)");


DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    
    debugLevel = 0;

    startupError = 0;

#ifdef _WIN32
    
    switch (_fileno(stdin)) {
        case -1: {
            fprintf(stderr, "WolframLibrary_initialize: stdin _fileno error, errno: %d\n", errno);
            startupError = FILENO_ERROR;
            return 0;
        }
        case -2: {
            fprintf(stderr, "WolframLibrary_initialize: stdin is not associated with an input stream\n");
            startupError = FILENO_NOT_STREAM;
            return 0;
        }
    }

    //
    // Set binary mode for stdin and stdout on Windows
    //
    
    auto result = _setmode(_fileno(stdin), _O_BINARY);
    switch (result) {
        case -1: {
            fprintf(stderr, "WolframLibrary_initialize: cannot set _O_BINARY mode on stdin, errno: %d\n", errno);
            startupError = SETMODE_ERROR;
            return 0;
        }
    }



    switch (_fileno(stdout)) {
        case -1: {
            fprintf(stderr, "WolframLibrary_initialize: stdout _fileno error, errno: %d\n", errno);
            startupError = FILENO_ERROR;
            return 0;
        }
        case -2: {
            fprintf(stderr, "WolframLibrary_initialize: stdout is not associated with an output stream\n");
            startupError = FILENO_NOT_STREAM;
            return 0;
        }
    }

    //
    // The note here:
    // https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/setmode?view=vs-2019
    // says:
    // "If you have not written data to the stream, you do not have to flush the code."
    //
//    if (fflush(stdout)) {
//        fprintf(stderr, "WolframLibrary_initialize: fflush failed before setting _O_BINARY mode on stdout\n");
//        startupError = FFLUSH_ERROR;
//        return 0;
//    }
    
    result = _setmode(_fileno(stdout), _O_BINARY);
    switch (result) {
        case -1: {
            fprintf(stderr, "WolframLibrary_initialize: cannot set _O_BINARY mode on stdout, errno: %d\n", errno);
            startupError = SETMODE_ERROR;
            return 0;
        }
    }
#endif // _WIN32

    return 0;
}


DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    
}


DLLEXPORT int SetDebug_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    debugLevel = static_cast<int>(MArgument_getInteger(Args[0]));

    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetStartupError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    MArgument_setInteger(Res, startupError);

    return LIBRARY_NO_ERROR;
}


DLLEXPORT int StartBackgroundReaderThread_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    readerThread = std::thread(threadBody);
    
    readerThread.detach();

    return LIBRARY_NO_ERROR;
}


//
// start a background thread to read from stdin
//
// this allows simple semantics for quickly checking if there is anything read
//
// canceling IO operations is very complicated and I would prefer to not enter that morass
//
// also, the thread itself understands Content-Length header and does efficient reading of many bytes
//
void threadBody() {
    
    std::string str;
    size_t numBytes = 0;
    
    std::unique_ptr<unsigned char[]> body;
    Message msg;
    
    int res;
    
    while (true) {
        
        if (debugLevel == DEBUG_VERBOSE) {
            fprintf(stderr, "threadBody: loop\n");
        }

        //
        // Read headers
        //
        
        std::vector<std::string> headers;
        
        while (true) {
            
            str.clear();
            
            res = readLineFromStdIn(str);
            
            if (res) {
                goto readThreadErr;
            }
            
            std::smatch m;
            
            if (str.empty()) {
                break;
            } else if (std::regex_match(str, m, ContentLengthRegEx)) {
                
                auto capture1 = m.str(1);
                
                numBytes = std::stoi(capture1, nullptr);
                
                headers.push_back(str);
                
            } else {
                
                res = UNRECOGNIZED_HEADER;
                
                goto readThreadErr;
            }
        } // while
        
        body = std::unique_ptr<unsigned char[]>(new unsigned char[numBytes]);
        
        res = readBytesFromStdIn(body.get(), numBytes);
        
        if (res) {
            goto readThreadErr;
        }
        
        msg = Message(headers, std::move(body), numBytes);
        
        if (debugLevel == DEBUG_VERBOSE) {
            fprintf(stderr, "native: threadBody: qMutex lock: before\n");
        }

        qMutex.lock();
        
        if (debugLevel == DEBUG_VERBOSE) {
            fprintf(stderr, "native: threadBody: qMutex lock: after\n");
        }

        q.push(std::move(msg));
        
        if (debugLevel == DEBUG_VERBOSE) {
            fprintf(stderr, "native: threadBody: qMutex unlock: before\n");
        }

        qMutex.unlock();

        if (debugLevel == DEBUG_VERBOSE) {
            fprintf(stderr, "native: threadBody: qMutex unlock: after\n");
        }

    } // while
    
readThreadErr:
    
    //
    // construct a Message to contain the error code and put in queue
    //

    auto bodyStr = std::string("{\"method\": \"stdio/error\", \"code\": ") + std::to_string(res) + "}";

    auto contentLengthStr = std::string("Content-Length: ") + std::to_string(bodyStr.size());

    auto Headers = std::vector<std::string>({contentLengthStr});
    
    auto Body = std::unique_ptr<unsigned char[]>(new unsigned char[bodyStr.size()]);

    std::memcpy(Body.get(), bodyStr.c_str(), bodyStr.size());

    auto Size = bodyStr.size();

    msg = Message(Headers, std::move(Body), Size);
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: threadBody readThreadErr: qMutex lock: before\n");
    }

    qMutex.lock();
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: threadBody readThreadErr: qMutex lock: after\n");
    }

    q.push(std::move(msg));
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: threadBody readThreadErr: qMutex unlock: before\n");
    }

    qMutex.unlock();
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: threadBody readThreadErr: qMutex unlock: after\n");
    }

    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: threadBody: exit\n");
    }
}


DLLEXPORT int LockQueue_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: LockQueue: qMutex lock: before\n");
    }

    qMutex.lock();
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: LockQueue: qMutex lock: after\n");
    }

    //
    // this is immediately after potentially long-running code,
    // but do not need to check libData->AbortQ() here
    // because just returning immediately
    //

    return LIBRARY_NO_ERROR;
}


DLLEXPORT int UnlockQueue_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: UnlockQueue: qMutex lock: before\n");
    }

    qMutex.unlock();
    
    if (debugLevel == DEBUG_VERBOSE) {
        fprintf(stderr, "native: UnlockQueue: qMutex lock: after\n");
    }

    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetQueueSize_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    auto s = q.size();
    
    MArgument_setInteger(Res, s);
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetFrontMessageSize_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    const auto& msg = q.front();
    
    MArgument_setInteger(Res, msg.Size);
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int PopQueue_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    const auto& msg = q.front();
    
    MNumericArray na;
    na = MArgument_getMNumericArray(Args[0]);
    
    auto data = reinterpret_cast<unsigned char *>(libData->numericarrayLibraryFunctions->MNumericArray_getData(na));
    
    std::copy(msg.Body.get(), msg.Body.get() + msg.Size, data);
    
    q.pop();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(na);
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetStdInFEOF_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    MArgument_setInteger(Res, feof(stdin));
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetStdInFError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    MArgument_setInteger(Res, ferror(stdin));
    
    return LIBRARY_NO_ERROR;
}


//
// Precondition: str is ready to be written to
//
// Lines always end with \r\n on all platforms, so just do simple loop here
//
int readLineFromStdIn(std::string& str) {

    char c;
    while (true) {
        
        auto r = fread(&c, sizeof(char), 1, stdin);
        if (r != 1) {
            return FREAD_FAILED;
        }

        if (c == '\r') {
            break;
        }
        //
        // EOF can insert \n at the end
        //
        if (c == '\n') {
            
            return UNEXPECTED_LINEFEED;
        }
        str += c;
    }

    auto r = fread(&c, sizeof(char), 1, stdin);
    if (r != 1) {
        return FREAD_FAILED;
    }

    if (c != '\n') {
        return EXPECTED_LINEFEED;
    }
    
    return 0;
}


int readBytesFromStdIn(unsigned char *data, size_t numBytes) {

    auto r = fread(data, sizeof(unsigned char), numBytes, stdin);
    if (r != numBytes) {
        
        return FREAD_FAILED;
    }

    return 0;
}


DLLEXPORT int ReadLineFromStdIn_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    static std::string str;
    
    {
        int res;

        str.clear();
        
        res = readLineFromStdIn(str);

        if (res) {
            
            MArgument_setInteger(Res, res);

            return LIBRARY_NO_ERROR;
        }
    }

    {
        auto res = str.c_str();

        MArgument_setUTF8String(Res, const_cast<char *>(res));
    }

    return LIBRARY_NO_ERROR;
}


DLLEXPORT int WriteLineToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    static char newline[] = { '\r', '\n' };

    auto line = MArgument_getUTF8String(Args[0]);

    size_t lineLen = strlen(line);

    auto w = fwrite(line, sizeof(char), lineLen, stdout);
    if (w != lineLen) {

        libData->UTF8String_disown(line);

        MArgument_setInteger(Res, FWRITE_FAILED);
        
        return LIBRARY_NO_ERROR;
    }

    w = fwrite(newline, sizeof(char), 2, stdout);
    if (w != 2) {

        libData->UTF8String_disown(line);

        MArgument_setInteger(Res, FWRITE_FAILED);
        
        return LIBRARY_NO_ERROR;
    }

    if (fflush(stdout)) {

        libData->UTF8String_disown(line);

        MArgument_setInteger(Res, FFLUSH_FAILED);
        
        return LIBRARY_NO_ERROR;
    }

    libData->UTF8String_disown(line);

    MArgument_setInteger(Res, 0);
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int WriteBytesToStdOut_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    auto na = MArgument_getMNumericArray(Args[0]);

    auto numBytes = static_cast<size_t>(libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(na));

    auto data = reinterpret_cast<unsigned char *>(libData->numericarrayLibraryFunctions->MNumericArray_getData(na));

    auto w = fwrite(data, sizeof(unsigned char), numBytes, stdout);
    if (w != numBytes) {

        libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

        MArgument_setInteger(Res, FWRITE_FAILED);
        
        return LIBRARY_NO_ERROR;
    }

    if (fflush(stdout)) {

        libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

        MArgument_setInteger(Res, FFLUSH_FAILED);
        
        return LIBRARY_NO_ERROR;
    }

    libData->numericarrayLibraryFunctions->MNumericArray_disown(na);

    MArgument_setInteger(Res, 0);
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetStdOutFEOF_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    MArgument_setInteger(Res, feof(stdout));
    
    return LIBRARY_NO_ERROR;
}


DLLEXPORT int GetStdOutFError_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    MArgument_setInteger(Res, ferror(stdout));
    
    return LIBRARY_NO_ERROR;
}
