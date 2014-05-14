PROJECT = poe_router

DEPS = cowboy ranger
dep_cowboy = pkg://cowboy 0.9.0
dep_ranger = https://github.com/camshaft/ranger.git master

include erlang.mk
