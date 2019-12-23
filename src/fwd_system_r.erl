%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(fwd_system_r).

-export([describe/0]).
-export([locate/1]).
-export([links/1]).
-export([get/1]).
-export([to_representation/3]).

describe() -> #{
	uri => "/",
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
		{child, fwd_processes_r},
		{child, fwd_tables_r}
	], Req}.

get(Req) ->
	Info = observer_backend:sys_info(),
	Data = #{
		<<"System and Architecture">> => #{
			<<"System Version">> => g(otp_release, Info),
			<<"ERTS Version">> => g(version, Info),
			<<"Compiled for">> => g(system_architecture, Info),
			<<"Emulator Wordsize">> => g(wordsize_external, Info),
			<<"Process Wordsize">> => g(wordsize_internal, Info),
			<<"SMP Support">> => g(smp_support, Info),
			<<"Thread Support">> => g(threads, Info),
			<<"Async thread pool size">> => g(thread_pool_size, Info)
		},
		<<"CPU's and Threads">> => #{
			<<"Logical CPU's">> => g(logical_processors, Info),
			<<"Online Logical CPU's">> => g(logical_processors_online, Info),
			<<"Available Logical CPU's">> => g(logical_processors_available, Info),
			<<"Schedulers">> => g(schedulers, Info),
			<<"Online schedulers">> => g(schedulers_online, Info),
			<<"Available schedulers">> => g(schedulers_available, Info)
		},
		<<"Memory Usage">> => #{ %% All bytes.
			<<"Total">> => g(total, Info),
			<<"Processes">> => g(processes, Info),
			<<"Atoms">> => g(atom, Info),
			<<"Binaries">> => g(binary, Info),
			<<"Code">> => g(code, Info),
			<<"ETS">> => g(ets, Info)
		},
		<<"Statistics">> => #{
			<<"Up time">> => g(uptime, Info), %% ms.
			<<"Run Queue">> => g(run_queue, Info),
			<<"IO Input">> =>  g(io_input, Info), %% bytes.
			<<"IO Output">> => g(io_output, Info) %% bytes.
		},
		<<"System statistics / limit">> => #{
			<<"Atoms">> => #{count => g(atom_count, Info), limit => g(atom_limit, Info)},
			<<"Processes">> => #{count => g(process_count, Info), limit => g(process_limit, Info)},
			<<"Ports">> => #{count => g(port_count, Info), limit => g(port_limit, Info)},
			<<"ETS">> => #{count => g(ets_count, Info), limit => g(ets_limit, Info)},
			<<"Distribution buffer busy limit">> => case g(dist_buf_busy_limit, Info) of
				undefined -> <<"Not available">>;
				V -> V
			end
		}
	},
	{ok, Data, Req}.

to_representation(Req, html, Data) ->
	{ok, farwest_html:from_term(Req, Data), Req};
to_representation(Req, bed, Data) ->
	{ok, farwest_bed:from_term(Req, Data), Req}.

g(Name, List) ->
	case lists:keyfind(Name, 1, List) of
		false -> undefined;
		{_, Value} when is_list(Value) -> list_to_binary(Value);
		{_, Value} -> Value
	end.
