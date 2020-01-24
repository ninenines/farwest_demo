%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(fwd_table_row_r).

-export([describe/0]).
-export([locate/1]).
-export([links/1]).
-export([get/1]).
-export([to_representation/3]).
-export([put/1]).
-export([delete/1]).

describe() -> #{
	uri => "/tables/{name}/{key}",
	media_types => #{
		html => ["text/html"],
		term => ["text/plain"]
	},
	operations => #{
		get => #{output => [html]},
		put => #{input => [term]},
		delete => #{}
	}
}.

%% @todo Not necessarily.
%% @todo forbidden if private or protected
locate(Req) ->
	{found, Req}.

links(Req) ->
	{ok, [
		%% @todo Here we should provide bindings as well to rebuild the URI when necessary,
		%% since our exact parent depends on the bindings we have.
		{parent, fwd_table_r}
	], Req}.

get(Req=#{bindings := #{name := Name0, key := Key0}}) ->
	Name = binary_to_atom(Name0, utf8),
	{ok, Key} = parse_string(unicode:characters_to_list(Key0) ++ "."),
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
	{Rows0, _} = receive_table(BackendPid, {[], 0}),
	%% We only keep the requested key.
	[Row] = [R || R <- Rows0, element(KeyPos, R) =:= Key], %% @todo This crashes if key is incorrect.
	{ok, Row, Req}.

to_representation(Req, html, Row) ->
	Data = {'$fw_tab', tuple_size(Row), stringify([Row])},
	{ok, farwest_html:from_term(Req, Data), Req}.

put(Req0=#{bindings := #{name := Name0, key := _Key0}}) ->
	Name = binary_to_atom(Name0, utf8),
%% @todo We probably should enforce the key.
%	{ok, Key} = parse_string(unicode:characters_to_list(Key0) ++ "."),
	{ok, Body, Req} = cowboy_req:read_body(Req0),
	{ok, Tuple} = parse_string(unicode:characters_to_list(Body) ++ "."),
	%% @todo May require id.
	ets:delete_object(Name, Tuple),
	ets:insert(Name, Tuple),
	{ok, Req}.

%% @todo Observer does a delete_object but it doesn't work very well for us
%% since we don't want to pass the entire tuple value in the URI.
delete(Req=#{bindings := #{name := Name0, key := Key0}}) ->
	Name = binary_to_atom(Name0, utf8),
	{ok, Key} = parse_string(unicode:characters_to_list(Key0) ++ "."),
	ets:delete(Name, Key),
	{ok, Req}.

%% @todo Identical code as in fwd_table_r.
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

stringify([]) -> [];
stringify([Row|Tail]) -> [stringify_tuple(Row)|stringify(Tail)].

stringify_tuple(Tuple) ->
	list_to_tuple([iolist_to_binary(io_lib:format("~0p", [Term])) || Term <- tuple_to_list(Tuple)]).

%% Copy pasted from observer_lib.
parse_string(Str) ->
    try
	Tokens = case erl_scan:string(Str, 1, [text]) of
		     {ok, Ts, _} -> Ts;
		     {error, {_SLine, SMod, SError}, _} ->
			 throw(io_lib:format("~ts", [SMod:format_error(SError)]))
		 end,
	case erl_eval:extended_parse_term(Tokens) of
	    {error, {_PLine, PMod, PError}} ->
		throw(io_lib:format("~ts", [PMod:format_error(PError)]));
	    Res -> Res
	end
    catch
	throw:ErrStr ->
	    {error, ErrStr};
	_:_Err ->
	    {error, ["Syntax error in: ", Str]}
    end.
