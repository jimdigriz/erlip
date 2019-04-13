Erlang IP Address Library.

# Preflight

    git clone https://gitlab.com/jimdigriz/erlip.git

## `erlang.mk`

Add to your `Makefile`:

    DEPS += erlip
    dep_erlip = git https://gitlab.com/jimdigriz/erlip.git master

# Usage

## `erlip`

Contains a number of useful helpers:

You can convert an IP into an [`inet:ip_address()`](http://erlang.org/doc/man/inet.html#data-types)

    1> erlip:to_ip_address(<<"192.0.2.1">>).
    {192,0,2,1}

You can convert a CIDR to a range:

    1> erlip:to_ip_range(<<"192.0.2.111/24">>).
    {{192,0,2,0},{192,0,2,255}}

You can get the IP version:

    1> erlip:version(<<"192.0.2.1">>).
    4
    2> erlip:version("2001:db8:735e:1:6c9b:c6a9:a3c0:f136").
    6

You can obtain your external address:

    1> erlip:external().	% same as erlip:external(ipv4).
    {192,0,2,1}
    2> erlip:external(ipv6).
    {8193,3512,29534,1,27803,50857,41920,61750}

**N.B.** your DNS request may be subject to [local privacy policies and truncated](https://tools.ietf.org/html/rfc7871#section-11.1)

## `erlip_range`

An IP range structure implemented using [`gb_trees`](http://erlang.org/doc/man/gb_trees.html); a use case is as an IP blacklist checker.

    1> Range = erlip_range:from_list(["192.0.2.0/24", <<"198.51.100.0/24">>, {{203,0,113,0},{203,0,113,255}}, "2001:db8::/32"]).
    2> erlip_range:contains("192.0.2.1", Range).
    true
    3> erlip_range:contains(<<"192.168.1.1">>, Range).
    false
    4> erlip_range:contains("2001:db8:735e:1:6c9b:c6a9:a3c0:f136", Range).
    true

After doing many (overlapping) inserts, you should garbage collect the range:

    Range = erlip_range:gc(Range0).

You can also merge ranges:

    1> RangeA0 = erlip_range:new().
    2> RangeA = erlip_range:insert(<<"192.0.2.0/24">>, RangeA0).
    3> RangeB0 = erlip_range:new().
    4> RangeB = erlip_range:insert(["2001:db8::/32",{{203,0,113,0},{203,0,113,255}}], RangeB0).
    5) Range = erlip_range:merge([RangeA,RangeB]).

**N.B.** you should call `gc/1` after using merge

### Performance

A large list (no overlaps and pre-aggregated) with ~40k entries on an [Intel i7-8550U](https://ark.intel.com/content/www/us/en/ark/products/122589/intel-core-i7-8550u-processor-8m-cache-up-to-4-00-ghz.html) I get the following results:

    1> {ok, F} = file:read_file("list").
    {ok,<<"52.215.197.128/27\r\n178.174.40.176/30\r\n195.12.50.236/31\r\n...>>}
    
    2> L = lists:droplast(binary:split(F, <<"\r\n">>, [global])).
    [<<"52.215.197.128/27">>,<<"178.174.40.176/30">>,<<"195.12.50.236/31">>|...]
    
    3> length(L).
    41263
    
    4> {T, R} = timer:tc(fun() -> erlip_range:from_list(L) end).
    {282375, ...}       <--- 280ms
    
    5> gb_trees:size(R).
    41263
    
    6> timer:tc(fun() -> erlip_range:contains({1,2,3,4}, R) end).
    {29,false}          <--- 0.03ms
    
    7> timer:tc(fun() -> erlip_range:contains({109,200,217,123}, R) end).
    {31,true}           <--- 0.03ms

## `erlip_list`

Provides lists of IP ranges you may find useful.

[RFC 6890](https://tools.ietf.org/html/rfc6890) (superset of [RFC 1918](https://tools.ietf.org/html/rfc1918)) provides a list useful for filtering addresses you should never see:

    1> erlip_list:rfc6890().

# Development

To quickly get a prompt to start playing with this:

    make all shell
