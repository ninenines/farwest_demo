%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>

-module(farwest_demo_config).

-export([resource_modules/0]).

resource_modules() -> [
	fwd_system_r,
	fwd_processes_r,
	fwd_process_r,
	fwd_tables_r,
	fwd_table_r,
	fwd_table_row_r
].
