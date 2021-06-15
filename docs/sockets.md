
## Restart Servers

Doing "LSP: Restart Servers" in Sublime returns "address already in use" errors

### Description

Sublime tries to connect to the same port before the kernel closes down.


## Sublime

### Timeout issue

If LSPServer takes more than 5 seconds to launch Sublime returns "time out" error.

* Timeout issue reported in [sublimelsp git](https://github.com/sublimelsp/LSP/issues/622).

* Text from [Sublime Text Language server protocol documentation](https://lsp.readthedocs.io/en/latest/).

    > Set ```tcp_mode``` to "host", leave ```tcp_port``` unset for automatic port selection. ```tcp_port``` can be set if eg. debugging a server. You may want to check out the LSP source and extend the ```TCP_CONNECT_TIMEOUT```.

* [Sublimelsp sourcecode for timeout](https://github.com/sublimelsp/LSP/blob/master/plugin/core/transports.py#L18).

### Socket support
* [Client-hosted tcp connection and client managing the socket life cycle](https://github.com/sublimelsp/LSP/issues/513)

## VSCode

### Opening the port
* [For connection timing issues the server is actually a client and the client is the server in terms of opening the ports.](https://github.com/microsoft/language-server-protocol/issues/604)


## Multiple clients support plans

* [Plans for supporting multiple clients from a single LSP server?](https://github.com/microsoft/language-server-protocol/issues/1160)
