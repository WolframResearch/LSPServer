
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

# VSCode

## Important links

### Opening the port
* [For connection timing issues the server is actually a client and the client is the server in terms of opening the ports.](https://github.com/microsoft/language-server-protocol/issues/604)


## Multiple clients support plans

* [Plans for supporting multiple clients from a single LSP server?](https://github.com/microsoft/language-server-protocol/issues/1160)

## Setting for Socket based communication
We are working to get Socket support for our VSCode plugin. 
