# ePort application

ePort is a library to make easy but restricted rpc:calls between erlang components
running on diffrent nodes.

The normal rpc module uses node communication to reply with its result which is 
not possible to use if the two components that should be communicating is run by
diffrent users.

##How to connect

### The listener side
The listener side starts upp an ePortListener which listens for incoming 
connections (default port 19000)

    ePortListener:start_link(Module, Port, AllowedIPs)
    
    Module     = Name of protocol module, see: eProtocol
    Port       = Port to listen to incoming connections on.
    AllowedIPs = List of IP-addresses which the listener accepts incoming 
                 connections to (optional)
                 
    Example:

    {ok, Pid} = ePortListener:start_link(srvProtModule, 19000).                            

### The connecting side                 
The connecting side starts ePort which connects to a host at a port.                 
                 
    ePort:start_link(Module, Host, Port)
    
    Module     = Name of protocol module for client side, see: eProtocol
    Host       = IP address or computer to connect too
    Port       = Port to connect too
    
    Example:
    
    {ok, Pid} = ePort:start_link(clientProtModule, myserver.com, 19000).

When the connecting call from ePort to ePortListener is done, eportListener
calls the function srvProtModule:clientConnected, see eProtocol.erl. When 
epostListener detects that the socket is down it calls srvProtModule:clientDisconnected.

The server side receives a Pid to use for outgoing communication.

ePort client can only call functions defined in the ePortListeners srvProtModule.
ePort server can only call functions defined in the ePorts clientProtModule.

### Simple example

To setup a server at IP: 192.168.1.1 that allows the use of the lists module you 
need to do the following on the server side.

    {ok, SrvPid} = ePortListener:start_link(lists, 19000).
    
To execute a functions using that eport listener:

    {ok, ClientPid} = ePort:start_link(undefined, "192.168.1.1", 19000),
    ePort:call(ClientPid, append, [[1,2], [3, 4]]).
    
To setup a connection which allows the same server to call the clients io module.

On the server side

    {ok, SrvPid} = ePortListener:start_link(eProtocol, 19000).

On the client side

    {ok, ClientPid} = ePort:start_link(io, "192.168.1.1", 19000).
    
On the server side the function clientConnected will be called in the servers 
protocol module. This call includes the pid for the ePort connection.

    clientConnected(EPortPid, EListenerPid, PeerHost) ->
    
Save the pid EPortPid and use it on the server side to call the client:

    ePort:call(EPortPid, format, ["Test of output"]).
    
### Last but least...

Apart from doing call:s using ePort you can also do cast:s in the same way
as described above.

It is also possible to setup encrypted communication, see ePort for an example of
how to do that.

    
    