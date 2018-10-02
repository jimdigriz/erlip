-module(erlip).

-export([to_ip_address/1]).
-export([to_ip_range/1]).
-export([version/1]).
-export([external/0, external/1]).

-type ip() :: inet:ip_address() | string().

-export_type([ip/0]).

-spec to_ip_address(erlip:ip()) -> inet:ip_address().
to_ip_address(IP) when is_binary(IP) ->
	to_ip_address(binary_to_list(IP));
to_ip_address(IP) when is_list(IP) ->
	{ok, IPAddress} = inet:parse_strict_address(IP),
	to_ip_address(IPAddress);
to_ip_address(IPAddress) ->
	true = is_list(inet:ntoa(IPAddress)),
	IPAddress.

-spec to_ip_range(string()) -> {inet:ip_address(),inet:ip_address()}.
to_ip_range(CIDR) when is_binary(CIDR) ->
	[IP|Mask0] = binary:split(CIDR, <<"/">>),
	IPAddress = to_ip_address(IP),
	Mask = case Mask0 of
		[] when size(IPAddress) == 4 ->
			32;
		[] when size(IPAddress) == 8 ->
			128;
		[M] ->
			binary_to_integer(M)
	end,
	to_ip_range(IPAddress, Mask);
to_ip_range(CIDR) when is_list(CIDR) ->
	to_ip_range(list_to_binary(CIDR)).
to_ip_range({Low,High}, Mask) when size(Low) == 8 andalso Mask == 128; size(Low) == 4 andalso Mask == 32 ->
	MaskRange = lists:map(fun(_) -> 0 end, lists:seq(1,size(Low)-length(High))) ++ High,
	Range = trunc(math:pow(2, 2*size(Low))) - 1,
	Low2 = lists:zipwith(fun(X, Y) -> X band (Range - Y) end, tuple_to_list(Low), MaskRange),
	High2 = lists:zipwith(fun(X, Y) -> X + Y end, Low2, MaskRange),
	{list_to_tuple(Low2),list_to_tuple(High2)};
to_ip_range({Low,High}, Mask) ->
	Width = size(Low) * 2,
	Mask2 = Width - Mask rem Width,
	to_ip_range({Low, High ++ [trunc(math:pow(2, Mask2)) - 1]}, Mask + Mask2);
to_ip_range(Subnet, Mask) ->
	to_ip_range({Subnet,[]}, Mask).

-spec version(erlip:ip()) -> 4 | 6.
version(IP) ->
	IPAddress = to_ip_address(IP),
	case size(IPAddress) of
		X when X == 4 -> 4;
		X when X == 8 -> 6
	end.

% https://code.blogs.iiidefix.net/posts/get-public-ip-using-dns/
-spec external() -> inet:ip_address().
external() ->
	external(ipv4).

-spec external(ipv4 | ipv6) -> inet:ip_address().
% dig -4 TXT o-o.myaddr.l.google.com @ns1.google.com
external(ipv4) ->
	Targets = ["ns1.google.com", "ns2.google.com", "ns3.google.com", "ns4.google.com"],
	Keys = lists:map(fun(X) -> rpc:async_call(node(), inet_res, lookup, [X, in, a]) end, Targets),
	NS = lists:map(fun(X) -> {X,53} end, lists:flatmap(fun(X) -> rpc:yield(X) end, Keys)),
	external("o-o.myaddr.l.google.com", txt, NS);
% dig -6 AAAA myip.opendns.com @resolver1.ipv6-sandbox.opendns.com
external(ipv6) ->
	Targets = ["resolver1.ipv6-sandbox.opendns.com", "resolver1.ipv6-sandbox.opendns.com"],
	Keys = lists:map(fun(X) -> rpc:async_call(node(), inet_res, lookup, [X, in, aaaa]) end, Targets),
	NS = lists:map(fun(X) -> {X,53} end, lists:flatmap(fun(X) -> rpc:yield(X) end, Keys)),
	external("myip.opendns.com", aaaa, NS).

%%

external(H, Type, NS) ->
	RR = inet_res:lookup(H, in, Type, [{nameservers,NS}]),
	case RR of
		[] ->
			undefined;
		[[X]] ->	% txt
			to_ip_address(X);
		[X] ->		% a | aaaa
			X
	end.
