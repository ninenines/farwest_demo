PROJECT = farwest_demo
PROJECT_DESCRIPTION = Farwest Demo
PROJECT_VERSION = 0.1.0
PROJECT_ENV = [{farwest_config_module, farwest_demo_config}]

DEPS = $(if $(LOCAL_FARWEST),,farwest)
LOCAL_DEPS = $(if $(LOCAL_FARWEST),farwest)

dep_farwest = git https://github.com/ninenines/farwest master

RELX_TAR = 0

include erlang.mk
