-module(erlip_range).

-export([new/0]).
-export([from_list/1]).
-export([insert/2]).
-export([gc/1]).
-export([merge/1]).
-export([contains/2]).

-type range() :: gb_trees:gb_tree().

-export_type([range/0]).

-spec new() -> range().
new() ->
	gb_trees:empty().

-spec from_list(list(erlip:ip() | {erlip:ip(),erlip:ip()})) -> range().
from_list(Range = [IP|_]) when is_binary(IP); is_list(IP); is_tuple(IP) ->
	insert(Range, new()).

-spec insert(erlip:ip() | list(erlip:ip() | {erlip:ip(),erlip:ip()}), range()) -> range().
insert(Range = [IP|_], Tree) when is_binary(IP); is_list(IP); is_tuple(IP) ->
	lists:foldl(fun
		(I = {S,E}, T) when is_tuple(S), is_tuple(E), size(S) == size(E) ->
			S = erlip:to_ip_address(S),
			E = erlip:to_ip_address(E),
			insert2(I, T);
		(I, T) when is_binary(I); is_list(I) ->
			insert2(cidr2range(I), T);
		(I, T) when is_tuple(I) ->
			I = erlip:to_ip_address(I),
			insert2({I,I}, T)
	end, Tree, Range);
insert(IP, Tree) ->
	insert([IP], Tree).

-spec gc(range()) -> range().
gc(Range) ->
	gb_trees:balance(Range).

-spec merge(list(range())) -> range().
merge(Trees) when is_list(Trees) ->
	gc(lists:foldl(fun(T, TT) ->
		merge2(gb_trees:next(gb_trees:iterator(T)), TT)
	end, new(), Trees)).

% http://erlang.org/pipermail/erlang-questions/2015-November/thread.html#86776
-spec contains(erlip:ip(), range()) -> boolean().
contains(IP, Tree) ->
	IPAddress = erlip:to_ip_address(IP),
	contains(IPAddress, Tree, gb_trees:next(gb_trees:iterator_from(IPAddress, Tree))).

%%

insert2({Start, End}, Tree) ->
	insert2({Start, End}, Tree, gb_trees:next(gb_trees:iterator_from(Start, Tree))).
insert2({Start, End}, Tree, {XEnd, XStart, _}) when XStart =< Start, XEnd >= End ->
	Tree;
insert2({Start, End}, Tree, {XEnd, XStart, _}) when Start =< XStart, End >= XEnd ->
	insert2({Start, End}, gb_trees:delete(XEnd, Tree));
insert2({Start, End}, Tree, {XEnd, XStart, _}) when XStart =< Start, End >= XEnd ->
	insert2({XStart, End}, gb_trees:delete(XEnd, Tree));
insert2({Start, End}, Tree, _) ->
	gb_trees:insert(End, Start, Tree).

merge2({End, Start, Iter}, Tree) ->
	merge2(gb_trees:next(Iter), insert2({Start, End}, Tree));
merge2(none, Tree) ->
	Tree.

cidr2range(CIDR) when is_binary(CIDR) ->
	[IP|Mask0] = binary:split(CIDR, <<"/">>),
	IPAddress = erlip:to_ip_address(IP),
	Mask = case Mask0 of
		[] when size(IPAddress) == 4 ->
			<<"32">>;
		[] when size(IPAddress) == 8 ->
			<<"128">>;
		[M] ->
			M
	end,
	cidr2range(IPAddress, binary_to_integer(Mask));
cidr2range(CIDR) when is_list(CIDR) ->
	cidr2range(list_to_binary(CIDR)).
cidr2range({Low,High}, Mask) when size(Low) == 8 andalso Mask == 128; size(Low) == 4 andalso Mask == 32 ->
	MaskRange = lists:map(fun(_) -> 0 end, lists:seq(1,size(Low)-length(High))) ++ High,
	Range = trunc(math:pow(2, 2*size(Low))) - 1,
	Low2 = lists:zipwith(fun(X, Y) -> X band (Range - Y) end, tuple_to_list(Low), MaskRange),
	High2 = lists:zipwith(fun(X, Y) -> X + Y end, Low2, MaskRange),
	{list_to_tuple(Low2),list_to_tuple(High2)};
cidr2range({Low,High}, Mask) ->
	Width = size(Low) * 2,
	Mask2 = Width - Mask rem Width,
	cidr2range({Low, High ++ [trunc(math:pow(2, Mask2)) - 1]}, Mask + Mask2);
cidr2range(Subnet, Mask) ->
	cidr2range({Subnet,[]}, Mask).

contains(_IP, _Tree, none) ->
	false;
contains(IP, _Tree, {_, Start, _}) when Start > IP ->
	false;
contains(_IP, _Tree, _) ->
	true.
