PROJECT = poe_router

DEPS = cowboy ranger simple_env
dep_cowboy = pkg://cowboy 0.9.0
dep_ranger = https://github.com/camshaft/ranger.git master
dep_simple_env = https://github.com/camshaft/simple_env.git master

include erlang.mk
