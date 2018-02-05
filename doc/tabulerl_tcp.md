

# Module tabulerl_tcp #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td>Establish a connection to a SQL Server host.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-1"></a>

### connect/1 ###

<pre><code>
connect(Url::<a href="tabulerl_url.md#type-url">tabulerl_url:url()</a>) -&gt; {ok, <a href="inet.md#type-socket">inet:socket()</a>} | {error, string()}
</code></pre>
<br />

Establish a connection to a SQL Server host. Takes a url,
parses it using tabulerl_url:parse/1, and returns an inet:socket().

__See also:__ [gen_tcp:connect/3](gen_tcp.md#connect-3).

