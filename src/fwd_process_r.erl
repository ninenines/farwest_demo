%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(fwd_process_r).

-export([describe/0]).
-export([locate/1]).
-export([links/1]).
-export([get/1]).
-export([to_representation/3]).

%% Shared with fwd_processes_r.
-export([mfa/2]).
-export([info/2]).

describe() -> #{
	%% By making the router support uri templates we could avoid
	%% this repetition and perhaps use more powerful routing.
	uri => "/processes/:pid",
	uri_template => "/processes/{pid}",
	constraints => [{pid, fun pid_constraint/2}],
	media_types => #{
		html => ["text/html"]
	},
	operations => #{
		get => #{output => [html]}
	}
}.

pid_constraint(forward, Bin) ->
	try
		{ok, list_to_pid(binary_to_list(Bin))}
	catch _:_ ->
		{error, not_a_pid}
	end;
pid_constraint(reverse, Pid) when is_pid(Pid) ->
	{ok, pid_to_list(Pid)};
pid_constraint(reverse, _) ->
	{error, not_a_pid};
pid_constraint(format_error, {not_a_pid, NotAPid}) ->
	io_lib:format("The value ~p is not a pid.", [NotAPid]).

%% We may be able to detect if the process
%% previously existed based on self()?
%% But that would not be entirely reliable.
locate(Req=#{bindings := #{pid := Pid}}) ->
	case is_process_alive(Pid) of
		true -> {found, Req};
		false -> {not_found, Req}
	end.

links(Req) ->
	{ok, [
		{parent, fwd_processes_r}
	], Req}.

get(Req=#{bindings := #{pid := Pid}}) ->
	Data = #{
		%% Overview.
		<<"Initial call">> => mfa(Pid, initial_call),
		<<"Current function">> => mfa(Pid, current_function),
		<<"Registered name">> => info(Pid, registered_name),
		<<"Status">> => info(Pid, status),
		<<"Message queue length">> => info(Pid, message_queue_len),
		<<"Group leader">> => linkify(info(Pid, group_leader)),
		<<"Priority">> => info(Pid, priority),
		<<"Trap exits">> => info(Pid, trap_exit),
		<<"Reductions">> => info(Pid, reductions),
		%%<<"Binary">>
		%%<<"Last calls">>
		%%<<"Catch level">>
		%%<<"Trace">>
		%%<<"Suspending">>
		%%<<"Sequential trace token">>
		<<"Error handler">> => info(Pid, error_handler),

		%% Related processes.
		<<"Links">> => linkify(info(Pid, links)),
		<<"Monitors">> => linkify(info(Pid, monitors)),
		<<"Monitored by">> => linkify(info(Pid, monitored_by))

		%% Memory and garbage collection.
		%%<<"Memory">>
		%%<<"Stack and heaps">>
		%%<<"Heap size">>
		%%<<"Stack size">>
		%%<<"GC min heap size">>
		%%<<"GC fullsweep after">>
	},
	{ok, Data, Req}.

to_representation(Req, html, Data) ->
	{ok, farwest_html:from_term(Req, Data), Req}.

info(Pid, Name) ->
	case erlang:process_info(Pid, Name) of
		[] -> '';
		{_, Value} -> Value
	end.

mfa(Pid, Name) ->
	{M,F,A} = info(Pid, Name),
	iolist_to_binary([
		atom_to_binary(M, utf8),
		$:,
		atom_to_binary(F, utf8),
		$/,
		integer_to_binary(A)
	]).

linkify(Pid) when is_pid(Pid) -> {'$fw_link', child, ["/processes/", cow_uri:urlencode(list_to_binary(pid_to_list(Pid)))], Pid};
linkify(List) when is_list(List) -> [linkify(Item) || Item <- List];
linkify(Other) -> Other.
