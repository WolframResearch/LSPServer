
# Sublime Text (ST)

## Unsolved issues

### 1. Restart Servers

Doing "LSP: Restart Servers" in ST returns "address already in use" errors. As ST tries to connect to the same port before the kernel closes down ST finds the port is already in use. 

Wolfram kernel takes few seconds to close down. Opening ST after kernel close restarts the server and works properly.

### 2. Timeout issue

If LSPServer takes more than 5 seconds to launch Sublime returns "time out" error.

* Timeout issue reported in [sublimelsp git](https://github.com/sublimelsp/LSP/issues/622).

* Text from [Sublime Text Language server protocol documentation](https://lsp.readthedocs.io/en/latest/).

    > Set ```tcp_mode``` to "host", leave ```tcp_port``` unset for automatic port selection. ```tcp_port``` can be set if eg. debugging a server. You may want to check out the LSP source and extend the ```TCP_CONNECT_TIMEOUT```.

* [Sublimelsp sourcecode for timeout](https://github.com/sublimelsp/LSP/blob/master/plugin/core/transports.py#L18).

## Important links
* [Client-hosted tcp connection and client managing the socket life cycle](https://github.com/sublimelsp/LSP/issues/513)

## Setting for Socket based communication
Settings for socket based communication is given below:

### When client opens the port

Modify ```/Users/user-name/Library/Application Support/Sublime Text 3/Packages/User/LSP.sublime-settings``` file. We call this ```Socket``` mode of communication.

We can modify this file from Sublime Text menu : ```Sublime Text > Preferences > Package Settings > LSP > Settings```.

Setting for communication through ```Socket``` mode:

```{
  "log_debug": true,
  "log_server": true,
  "log_stderr": true,
  "log_payloads": true,
  "clients":
  {
    "wolfram":
    {
      "enabled": true,
      "command":
        [
          "C:\\Program Files\\Wolfram Research\\Mathematica\\12.1.1\\WolframKernel.exe",
          "-noinit",
          "-noprompt",
          "-nopaclet",
          "-noicon",
          "-run",
          "Needs[\"LSPServer`\"];LSPServer`StartServer[\"CommunicationMethod\" -> \"Socket\"]"
        ],
      "scopes": ["source.wolfram"],
      "syntaxes": "/Users/suman/Documents/WRI/External-Repo/Sublime-WolframLanguage/WolframLanguage.sublime-syntax",
      "languageId": "wolfram",
      "initializationOptions": { },
      "tcp_mode": "host",
      "tcp_port": 5555
    }
  }
}
```
We recommend using Mathematica version 12.1.1 for this mode. Although sometimes ST socket support works properly with later versions but we see the following issues to be fixed: 

* Server successfully connects to the socket opened by the client but never reads a message from the client.
* Server fails to connect to the socket opened by the client and returns ```Failed socket operation```.


### When server opens the port

Modify ```/Users/user-name/Library/Application Support/Sublime Text 3/Packages/User/LSP.sublime-settings``` file. We call this ```ListenSocket``` mode of communication.

We can modify this file from Sublime Text menu : ```Sublime Text > Preferences > Package Settings > LSP > Settings```.

Setting for communication through ```ListenSocket``` mode:

```{
  "log_debug": true,
  "log_server": true,
  "log_stderr": true,
  "log_payloads": true,
  "clients":
  {
    "wolfram":
    {
      "enabled": true,
      "command":
        [
          "C:\\Program Files\\Wolfram Research\\Mathematica\\12.1.1\\WolframKernel.exe",
          "-noinit",
          "-noprompt",
          "-nopaclet",
          "-noicon",
          "-run",
          "Needs[\"LSPServer`\"];LSPServer`StartServer[\"CommunicationMethod\" -> \"ListenSocket\"]"
        ],
      "scopes": ["source.wolfram"],
      "syntaxes": "/Users/suman/Documents/WRI/External-Repo/Sublime-WolframLanguage/WolframLanguage.sublime-syntax",
      "languageId": "wolfram",
      "initializationOptions": { },
      "tcp_mode": "",
      "tcp_port": 5555
    }
  }
}
```
This mode is used to support multi-client communication with single ```LSPServer```.

## Multiple clients support

As communication through ```ListenSocket``` mode is possible in ST, ```LSPServer``` is capable of multiple client support. Currently this feature is working for ST in Mac-OS-X. Initial tests of this feature is done using three different instances of STs. We have seen all the three STs could communicate with the ```LSPServer```.

### I. Client setting for Multiple clients support

All of the different STs should have the same setting as described in the sub-section: ```When server opens the port``` under ```Setting for Socket based communication```.

### II. Timing Tests

Timing tests in the multi-client mode are done in `Sublime Text 3`. Settings and results of the timing tests are dicussed below:

#### Settings

Three different instances of ST3s are open in the `ListenSocket` mode. All the different instances of STs started communicating with `LSPServer`. In each of the instances `Round Trip Timing Test` were done. 

#### results
Typical `Round Trip Timing` data and correponding data range is shown here. No significant delay was observed while switching the window and getting responses from the `LSPServer`.
__________________________________________________________
|         | ST3-Window-1 | ST3-Window-2 | ST3-Window-3 |
|---      | --- | --- | --- |
|Round Trip Timing   | 20 ms| 20 ms| 20 ms|
|Data Range      | 10 - 30 ms | 10 - 30 ms| 10 - 30 ms|
__________________________________________________________

### III. Issues
* Socket timeout issue comes sometime especially in the slower machines. Restarting ST generally solve this sissue. Improvement in ST4 may solve this issue.

* All the different instances of the ST3 opens a WolframKernel. Only the first kernel lives. The second and third kernel closes down with error message: `Address already in use (code 48)`. So finally all the ST3 communicate through the kernel launched by the first ST3.

  TODO: If a WolframKernel is launched in the ListenSocket mode, we can try not to launch another kernel. 

* If any of the instances of the ST3 is closed then the kernel is shut down by the server. 

    TODO: This issue needs to be fixed in the server side.


# VSCode

## Important links

### Opening the port
* [For connection timing issues the server is actually a client and the client is the server in terms of opening the ports.](https://github.com/microsoft/language-server-protocol/issues/604)


## Multiple clients support plans

* [Plans for supporting multiple clients from a single LSP server?](https://github.com/microsoft/language-server-protocol/issues/1160)

## Setting for Socket based communication
We are working to get Socket support for our VSCode plugin. 
