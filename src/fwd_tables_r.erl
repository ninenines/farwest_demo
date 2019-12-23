%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(fwd_tables_r).

-export([describe/0]).
-export([locate/1]).
-export([links/1]).
-export([get/1]).
-export([to_representation/3]).

describe() -> #{
	uri => "/tables",
	media_types => #{
		html => ["text/html"]
	},
	operations => #{
		get => #{output => [html]}
	}
}.

locate(Req) ->
	{found, Req}.

links(Req) ->
	{ok, [
		{parent, fwd_system_r},
		{child, fwd_table_r}
	], Req}.

get(Req) ->
	TableList = observer_backend:get_table_list(ets,
		[{sys_hidden, false}, {unread_hidden, false}]),
	{ok, TableList, Req}.

to_representation(Req, html, TableList) ->
	Data = {'$fw_tab',
		[<<"Table Name">>, <<"Objects">>, <<"Size (kB)">>,
			<<"Owner Pid">>, <<"Owner Name">>, <<"Table Id">>],
		[#{
			<<"Table Name">> => {'$fw_link', child,
				["/tables/", atom_to_binary(g(name, Table), utf8)],
				g(name, Table)},
			<<"Objects">> => g(size, Table),
			<<"Size (kB)">> => g(memory, Table),
			<<"Owner Pid">> => g(owner, Table),
			<<"Owner Name">> => g(reg_name, Table),
			<<"Table Id">> => case g(id, Table) of ignore -> <<>>; ID -> ID end
		} || Table <- TableList]
	},
	{ok, farwest_html:from_term(Req, Data), Req}.

g(Name, List) ->
	case lists:keyfind(Name, 1, List) of
		false -> undefined;
		{_, Value} -> Value
	end.
