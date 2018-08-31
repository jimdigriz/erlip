-module(erlip_list).

-export([rfc6890/0]).

% https://tools.ietf.org/html/rfc6890
-spec rfc6890() -> list(erlip:ip()).
rfc6890() -> [
	{{0,0,0,0},{0,255,255,255}},							% 0.0.0.0/8
	{{10,0,0,0},{10,255,255,255}},							% 10.0.0.0/8
	{{100,64,0,0},{100,127,255,255}},						% 100.64.0.0/10
	{{127,0,0,0},{127,255,255,255}},						% 127.0.0.0/8
	{{169,254,0,0},{169,254,255,255}},						% 169.254.0.0/16
	{{172,16,0,0},{172,31,255,255}},						% 172.16.0.0/12
	{{192,0,0,0},{192,0,0,255}},							% 192.0.0.0/24
	{{192,0,0,0},{192,0,0,7}},							% 192.0.0.0/29
	{{192,0,2,0},{192,0,2,255}},							% 192.0.2.0/24
	{{192,88,99,0},{192,88,99,255}},						% 192.88.99.0/24
	{{192,168,0,0},{192,168,255,255}},						% 192.168.0.0/16
	{{198,18,0,0},{198,19,255,255}},						% 198.18.0.0/15
	{{198,51,100,0},{198,51,100,255}},						% 198.51.100.0/24
	{{203,0,113,0},{203,0,113,255}},						% 203.0.113.0/24
	{{240,0,0,0},{255,255,255,255}},						% 240.0.0.0/4
	{{255,255,255,255},{255,255,255,255}},						% 255.255.255.255/32
	{{0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,1}},						% ::1/128
	{{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}},						% ::/128
	{{100,65435,0,0,0,0,0,0},{100,65435,0,0,0,0,65535,65535}},			% 64:ff9b::/96
	{{0,0,0,0,0,65535,0,0},{0,0,0,0,0,65535,65535,65535}},				% ::ffff:0:0/96
	{{256,0,0,0,0,0,0,0},{256,0,0,0,65535,65535,65535,65535}},			% 100::/64
	{{8193,0,0,0,0,0,0,0},{8193,511,65535,65535,65535,65535,65535,65535}},		% 2001::/23
	{{8193,0,0,0,0,0,0,0},{8193,0,65535,65535,65535,65535,65535,65535}},		% 2001::/32
	{{8193,2,0,0,0,0,0,0},{8193,2,0,65535,65535,65535,65535,65535}},		% 2001:2::/48
	{{8193,3512,0,0,0,0,0,0},{8193,3512,65535,65535,65535,65535,65535,65535}},	% 2001:db8::/32
	{{8193,16,0,0,0,0,0,0},{8193,31,65535,65535,65535,65535,65535,65535}},		% 2001:10::/28
	{{8194,0,0,0,0,0,0,0},{8194,65535,65535,65535,65535,65535,65535,65535}},	% 2002::/16
	{{64512,0,0,0,0,0,0,0},{65023,65535,65535,65535,65535,65535,65535,65535}},	% fc00::/7
	{{65152,0,0,0,0,0,0,0},{65215,65535,65535,65535,65535,65535,65535,65535}}	% fe80::/10
].
