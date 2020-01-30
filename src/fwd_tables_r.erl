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
		html => ["text/html"],
		bed => ["application/x-bed"]
	},
	operations => #{
		get => #{output => [html, bed]}
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
	Data = [#{
		<<"Table Name">> => case g(protection, Table) of
			private -> g(name, Table);
			_ ->
				{'$fw_link', child,
					farwest:link_to(fwd_table_r, #{<<"name">> => atom_to_binary(g(name, Table), utf8)}),
					g(name, Table)}
		end,
		<<"Objects">> => g(size, Table),
		<<"Size (kB)">> => g(memory, Table),
		<<"Owner Pid">> => g(owner, Table),
		<<"Owner Name">> => g(reg_name, Table),
		<<"Table Id">> => case g(id, Table) of ignore -> <<>>; ID -> ID end
	} || Table <- TableList],
	{ok, Data, Req}.

to_representation(Req, html, Data0) ->
	Data = {'$fw_tab',
		[<<"Table Name">>, <<"Objects">>, <<"Size (kB)">>,
			<<"Owner Pid">>, <<"Owner Name">>, <<"Table Id">>],
		Data0
	},
	{ok, farwest_html:from_term(Req, Data), Req};
to_representation(Req, bed, Data) ->
	{ok, farwest_bed:from_term(Req, Data), Req}.

g(Name, List) ->
	case lists:keyfind(Name, 1, List) of
		false -> undefined;
		{_, Value} -> Value
	end.
