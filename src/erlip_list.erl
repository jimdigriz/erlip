-module(erlip_list).

-export([rfc6890/0]).

% https://tools.ietf.org/html/rfc6890
-spec rfc6890() -> list(erlip:ip()).
rfc6890() -> [
	<<"0.0.0.0/8">>,
	<<"10.0.0.0/8">>,
	<<"100.64.0.0/10">>,
	<<"127.0.0.0/8">>,
	<<"169.254.0.0/16">>,
	<<"172.16.0.0/12">>,
	<<"192.0.0.0/24">>,
	<<"192.0.0.0/29">>,
	<<"192.0.2.0/24">>,
	<<"192.88.99.0/24">>,
	<<"192.168.0.0/16">>,
	<<"198.18.0.0/15">>,
	<<"198.51.100.0/24">>,
	<<"203.0.113.0/24">>,
	<<"240.0.0.0/4">>,
	<<"255.255.255.255/32">>,
	<<"::1/128">>,
	<<"::/128">>,
	<<"64:ff9b::/96">>,
	<<"::ffff:0:0/96">>,
	<<"100::/64">>,
	<<"2001::/23">>,
	<<"2001::/32">>,
	<<"2001:2::/48">>,
	<<"2001:db8::/32">>,
	<<"2001:10::/28">>,
	<<"2002::/16">>,
	<<"fc00::/7">>,
	<<"fe80::/10">>
].
