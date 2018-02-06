

# Module tabulerl_tcp #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Mostly just wraps around a handful of gen_tcp and inet functions.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close a TCP socket.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td>Establish a connection to a SQL Server host.</td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process to a socket.</td></tr><tr><td valign="top"><a href="#localname-1">localname/1</a></td><td>Get the local address and port for a given socket connection.</td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td>Get the address and port for a given socket connection.</td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td>Get the port for a given socket.</td></tr><tr><td valign="top"><a href="#recv-2">recv/2</a></td><td>Receive a packet from a socket Sock.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a packet on the socket Sock.</td></tr><tr><td valign="top"><a href="#shutdown-1">shutdown/1</a></td><td>Shuts down a TCP socket by signaling to the peer that no more data will be sent.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Sock::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; ok
</code></pre>
<br />

Close a TCP socket.

<a name="connect-1"></a>

### connect/1 ###

<pre><code>
connect(Url::<a href="tabulerl_url.md#type-url">tabulerl_url:url()</a>) -&gt; {ok, <a href="inet.md#type-socket">inet:socket()</a>} | {error, string()}
</code></pre>
<br />

Establish a connection to a SQL Server host. Takes a url, parses it using tabulerl_url:parse/1, and returns an inet:socket().

<a name="controlling_process-2"></a>

### controlling_process/2 ###

<pre><code>
controlling_process(Sock::<a href="inet.md#type-socket">inet:socket()</a>, Pid::pid()) -&gt; ok | {error, atom()}
</code></pre>
<br />

Assign a new controlling process to a socket.

<a name="localname-1"></a>

### localname/1 ###

<pre><code>
localname(Sock::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>
<br />

Get the local address and port for a given socket connection.

<a name="peername-1"></a>

### peername/1 ###

<pre><code>
peername(Sock::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>
<br />

Get the address and port for a given socket connection.

<a name="port-1"></a>

### port/1 ###

<pre><code>
port(Sock::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; {ok, <a href="inet.md#type-port_number">inet:port_number()</a>} | {error, any()}
</code></pre>
<br />

Get the port for a given socket.

<a name="recv-2"></a>

### recv/2 ###

<pre><code>
recv(Sock::<a href="inet.md#type-socket">inet:socket()</a>, Length::non_neg_integer()) -&gt; {ok, string() | binary() | term()} | {error, atom() | <a href="inet.md#type-posix">inet:posix()</a>}
</code></pre>
<br />

Receive a packet from a socket Sock.

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Sock::<a href="inet.md#type-socket">inet:socket()</a>, Packet::iolist()) -&gt; ok | {error, atom()}
</code></pre>
<br />

Send a packet on the socket Sock.

<a name="shutdown-1"></a>

### shutdown/1 ###

<pre><code>
shutdown(Sock::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; ok | {error, <a href="inet.md#type-posix">inet:posix()</a>}
</code></pre>
<br />

Shuts down a TCP socket by signaling to the peer that no more data will be sent.
Socket can still be read from until peer closes connection.

