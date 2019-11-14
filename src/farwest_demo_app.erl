%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(farwest_demo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', farwest:list_routes(farwest_demo)}
	]),
	{ok, _} = cowboy:start_clear(demo, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	farwest_demo_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(demo).
