PROJECT = farwest_demo
PROJECT_DESCRIPTION = Farwest Demo
PROJECT_VERSION = 0.1.0
PROJECT_ENV = [{farwest_config_module, farwest_demo_config}]

DEPS = farwest cowboy gun
dep_farwest = NOT CURRENTLY PUBLIC
dep_cowboy_commit = master
dep_gun_commit = master

RELX_TAR = 0

include erlang.mk
