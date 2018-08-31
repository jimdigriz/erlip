Erlang IP Address Library.

# Preflight

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

You can get the IP version:

    1> erlip:version(<<"192.0.2.1">>).
    4
    2> erlip:version("2001:db8:735e:1:6c9b:c6a9:a3c0:f136").
    6

You can use obtain your external IPv4 address:

    1> erlip:external().
    {192,0,2,1}

## `erlip_range`

An IP range structure implemented using [`gb_trees`](http://erlang.org/doc/man/gb_trees.html); a use case is as an IP blacklist checker.

    1> Range = erlip_range:from_list(["192.0.2.0/24", <<"198.51.100.0/24">>, {{203,0,113,0},{203,0,113,255}}, "2001:db8::/32"]).
    2> erlip_range:contains("192.0.2.1", Range).
    true
    3> erlip_range:contains(<<"192.168.1.1">>, Range).
    false
    4> erlip_range:contains("2001:db8:735e:1:6c9b:c6a9:a3c0:f136", Range).
    true

You can also merge ranges:

    1> RangeA0 = erlip_range:new().
    2> RangeA = erlip_range:insert(<<"192.0.2.0/24">>, RangeA0).
    3> RangeB0 = erlip_range:new().
    4> RangeB = erlip_range:insert(["2001:db8::/32",{{203,0,113,0},{203,0,113,255}}], RangeB0).
    5) Range = erlip_range:merge([RangeA,RangeB]).

## `erlip_list`

Provides lists of IP ranges you may find useful.

[RFC 6890](https://tools.ietf.org/html/rfc6890) (superset of [RFC 1918](https://tools.ietf.org/html/rfc1918)) provides a list useful for filtering addresses you should never see:

    1> erlip_range:rfc6890().

# Development

To quickly get a prompt to start playing with this:

    make all shell
