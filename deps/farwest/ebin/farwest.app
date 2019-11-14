{application, 'farwest', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{id, ""},
	{modules, ['farwest','farwest_app','farwest_auto_html','farwest_resource_h','farwest_sup']},
	{registered, [farwest_sup]},
	{applications, [kernel,stdlib,cowboy,gun]},
	{mod, {farwest_app, []}},
	{env, []}
]}.