PROJECT = farwest_demo
PROJECT_DESCRIPTION = Farwest Demo
PROJECT_VERSION = 0.1.0
PROJECT_ENV = [{farwest_config_module, farwest_demo_config}]

DEPS = farwest
dep_farwest = NOT CURRENTLY PUBLIC

RELX_TAR = 0

include erlang.mk
