Erlang IP Address Library.

# Usage

## `erlip_range`

An IP range structure implemented using [`gb_trees`](http://erlang.org/doc/man/gb_trees.html); a use case is as an IP blacklist checker.

    1> Range = erlip_range:from_list(["192.0.2.0/24", <<"198.51.100.0/24">>, {{203,0,113,0},{203,0,113,255}}, "2001:db8::/32"]).
    2> erlip_range:contains("192.0.2.1", Range).
    true
    3> erlip_range:contains(<<"192.168.1.1">>, Range).
    false
    4> erlip_range:contains("2001:db8:735e:1:6c9b:c6a9:a3c0:f136", Range).
    true
