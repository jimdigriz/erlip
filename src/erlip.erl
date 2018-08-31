-module(erlip).

-export([to_ip_address/1]).
-export([to_ip_range/1]).
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

-spec to_ip_range(string()) -> {inet:ip_address(),inet:ip_address()}.
to_ip_range(CIDR) when is_binary(CIDR) ->
	[IP|Mask0] = binary:split(CIDR, <<"/">>),
	IPAddress = to_ip_address(IP),
	Mask = case Mask0 of
		[] when size(IPAddress) == 4 ->
			<<"32">>;
		[] when size(IPAddress) == 8 ->
			<<"128">>;
		[M] ->
			M
	end,
	to_ip_range(IPAddress, binary_to_integer(Mask));
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
