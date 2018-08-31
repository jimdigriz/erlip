-module(erlip).

-export([to_ip_address/1]).
-export([version/1]).
-export([external/0]).

-type ip() :: inet:ip_address() | string().

-export_type([ip/0]).

-spec to_ip_address(erlip:ip()) -> inet:ip_address().
to_ip_address(IP) when is_binary(IP) ->
	to_ip_address(binary_to_list(IP));
to_ip_address(IP) when is_list(IP) ->
	{ok, IPAddress} = inet:parse_strict_address(IP),
	to_ip_address(IPAddress);
to_ip_address(IPAddress) ->
	false = is_tuple(inet:ntoa(IPAddress)),
	IPAddress.

-spec version(erlip:ip()) -> 4 | 6.
version(IP) ->
	IPAddress = to_ip_address(IP),
	case size(IPAddress) of
		X when X == 4 -> 4;
		X when X == 8 -> 6
	end.

-spec external() -> inet:ip_address().
external() ->
	% dig TXT o-o.myaddr.l.google.com @ns1.google.com
	Targets = ["ns1.google.com", "ns2.google.com", "ns3.google.com", "ns4.google.com"],
	Keys = lists:map(fun(X) -> rpc:async_call(node(), inet_res, lookup, [X, in, a]) end, Targets),
	Results = lists:map(fun(X) -> rpc:yield(X) end, Keys),
	NS = lists:filtermap(fun
		([I]) when is_tuple(I) -> {true, {I,53}};
		(_) -> false
	end, Results),
	RR = inet_res:lookup("o-o.myaddr.l.google.com", in, txt, [{nameservers,NS}]),
	case RR of
		[] ->
			undefined;
		[[X]] ->
			to_ip_address(X)
	end.
