%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(fwd_table_r).

-export([describe/0]).
-export([locate/1]).
-export([links/1]).
-export([get/1]).
-export([to_representation/3]).

describe() -> #{
	uri => "/tables/:name",
	media_types => #{
		html => ["text/html"]
	},
	operations => #{
		get => #{output => [html]}
	}
}.

%% @todo Not necessarily.
locate(Req) ->
	{found, Req}.

links(Req) ->
	{ok, [], Req}.

get(Req=#{bindings := #{name := Name0}}) ->
	Name = binary_to_atom(Name0, utf8),
	TableList = observer_backend:get_table_list(ets,
		[{sys_hidden, false}, {unread_hidden, false}]),
	[Table] = [T || T <- TableList, {name, Name} =:= lists:keyfind(name, 1, T)],
	%% @todo Handle the case when there's no id.
	Ref = case lists:keyfind(id, 1, Table) of
		{_, ignore} -> Name;
		{_, R} -> R
	end,
	KeyPos = ets:info(Name, keypos),
	BackendPid = observer_backend:get_table(self(), Ref, ets),
	{Rows, Cols} = receive_table(BackendPid, {[], 0}),
	{ok, {Rows, Cols, KeyPos}, Req}.

to_representation(Req=#{bindings := #{name := Name0}}, html, {Rows, Cols, KeyPos}) ->
	Data = {'$fw_tab', Cols, stringify(Rows, Name0, KeyPos)},
	{ok, farwest_auto_html:from_term(Req, Data), Req}.

receive_table(BackendPid, {Rows, Cols}) ->
	receive
		{BackendPid, '$end_of_table'} ->
			{Rows, Cols};
		{BackendPid, Records} ->
			receive_table(BackendPid, parse_ets_data(Records, Cols, Rows))
	after 5000 ->
		error(timeout)
	end.

%% Copy pasted from observer_tv_table.
parse_ets_data([[Rec]|Rs], C, Tab) ->
	parse_ets_data(Rs, max(tuple_size(Rec), C), [Rec|Tab]);
parse_ets_data([Recs|Rs], C0, Tab0) ->
	{Tab, Cols} = parse_ets_data(Recs, C0, Tab0),
	parse_ets_data(Rs, Cols, Tab);
parse_ets_data([], Cols, Tab) ->
	{Tab, Cols}.

stringify([], _, _) ->
	[];
stringify([Row|Tail], Name, KeyPos) ->
	[stringify_tuple(Row, Name, KeyPos)|stringify(Tail, Name, KeyPos)].

stringify_tuple(Tuple, Name, KeyPos) ->
	list_to_tuple([case Key of
		KeyPos -> linkify(element(Key, Tuple), Name);
		_ -> iolist_to_binary(io_lib:format("~0p", [element(Key, Tuple)]))
	end || Key <- lists:seq(1, tuple_size(Tuple))]).

linkify(Term, Name) ->
	{'$fw_link', child,
		[<<"/tables/">>, Name, <<"/">>,
			cow_qs:urlencode(iolist_to_binary(io_lib:format("~0p", [Term])))],
		iolist_to_binary(io_lib:format("~0p", [Term]))}.
