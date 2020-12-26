-module(erlip).

-export([to_ip_address/1]).
-export([to_ip_range/1]).
-export([to_integer/1]).
-export([version/1]).
-export([external/0, external/1]).

-type ip() :: inet:ip_address() | non_neg_integer() | string().

-export_type([ip/0]).

-spec to_ip_address(erlip:ip()) -> inet:ip_address().
to_ip_address(<<"::ffff:", IP/binary>>) ->
	inet:ipv4_mapped_ipv6_address(to_ip_address(IP));
to_ip_address("::ffff:" ++ IP) ->
	inet:ipv4_mapped_ipv6_address(to_ip_address(IP));
to_ip_address(IP) when is_binary(IP) ->
	to_ip_address(binary_to_list(IP));
to_ip_address(IP) when is_list(IP) ->
	{ok, IPAddress} = inet:parse_strict_address(IP),
	IPAddress;
to_ip_address(IP) when is_integer(IP), IP >= 16777216, IP < 4294967296 ->	% 1.0.0.0 -> 255.255.255.255
        to_ip_address(list_to_tuple([(IP bsr X) rem 256 || X <- lists:seq(32 - 8, -1, -8)]));
to_ip_address(IP) when is_integer(IP), IP >= 4294967296, IP < 340282366920938463463374607431768211456 ->
	to_ip_address(list_to_tuple([(IP bsr X) rem 65536 || X <- lists:seq(128 - 16, -1, -16)]));
to_ip_address(IPAddress) when is_tuple(IPAddress) ->
	{ok, IPAddress} = inet:parse_strict_address(inet:ntoa(IPAddress)),
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

-spec to_integer(erlip:ip()) -> non_neg_integer().
to_integer(IP) ->
	IPAddress = to_ip_address(IP),
	lists:sum(case size(IPAddress) of
		4 -> [ element(4 - (X div 8), IPAddress) bsl X || X <- lists:seq(32 - 8, -1, -8) ];
		8 -> [ element(8 - (X div 16), IPAddress) bsl X || X <- lists:seq(128 - 16, -1, -16) ]
	end).

-spec version(erlip:ip()) -> 4 | 6.
version(IP) ->
	IPAddress = to_ip_address(IP),
	case size(IPAddress) of
		4 -> 4;
		8 -> 6
	end.

% https://code.blogs.iiidefix.net/posts/get-public-ip-using-dns/
-spec external() -> inet:ip_address().
external() ->
	external(ipv4).

% https://developers.cloudflare.com/1.1.1.1/
% dig CH TXT whoami.cloudflare @1.1.1.1
% dig CH TXT whoami.cloudflare @2606:4700:4700::1111
-spec external(ipv4 | ipv6) -> inet:ip_address().
external(ipv4) ->
	external([{1,1,1,1},{1,0,0,1}]);
external(ipv6) ->
	external([{9734,18176,18176,0,0,0,0,4369},{9734,18176,18176,0,0,0,0,4097}]);
external(NS0) when is_list(NS0) ->
	NS = lists:map(fun(X) -> {X,53} end, NS0),
	RR = inet_res:lookup("whoami.cloudflare", chaos, txt, [{nameservers,NS}]),
	case RR of
		[] ->
			undefined;
		[[X]] ->	% txt
			to_ip_address(X);
		[_,["edns0-client-subnet " ++ X0]] ->	% [["2a04:e4c0:12::64"],["edns0-client-subnet 185.200.146.0/24"]]
			[X|_] = string:split(X0, "/"),
			to_ip_address(X);

		[X] ->		% a | aaaa
			X
	end.
