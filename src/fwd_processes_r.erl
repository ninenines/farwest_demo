%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(fwd_processes_r).

-export([describe/0]).
-export([locate/1]).
-export([links/1]).
-export([get/1]).
-export([to_representation/3]).

describe() -> #{
	uri => "/processes",
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
	{ok, [], Req}.

get(Req) ->
	{ok, [augment(Pid) || Pid <- erlang:processes()], Req}.

to_representation(Req, html, Data) ->
	{ok, farwest_auto_html:from_term(Req,
		{'$fw_tab',
			[<<"Pid">>, <<"Name or initial func">>, <<"Reductions">>,
				<<"Message queue length">>, <<"Current function">>],
			Data}), Req}.

augment(Pid) ->
	#{
		<<"Pid">> => linkify(Pid),
		<<"Name or initial func">> =>
			case fwd_process_r:info(Pid, registered_name) of
				'' -> fwd_process_r:mfa(Pid, initial_call);
				Name -> Name
			end,
		<<"Reductions">> => fwd_process_r:info(Pid, reductions),
		%%<<"Memory">>
		<<"Message queue length">> => fwd_process_r:info(Pid, message_queue_len),
		<<"Current function">> => fwd_process_r:mfa(Pid, current_function)
	}.

linkify(Pid) -> {'$fw_link', child, ["/processes/", cow_uri:urlencode(list_to_binary(pid_to_list(Pid)))], Pid}.
