PROJECT = poe_router

DEPS = cowlib cowboy ranger simple_env jsx jsxn export_val
dep_cowboy = pkg://cowboy 0.9.0
dep_cowlib = pkg://cowlib 0.6.1
dep_ranger = https://github.com/camshaft/ranger.git master
dep_simple_env = https://github.com/camshaft/simple_env.git master
dep_jsx = pkg://jsx develop
dep_jsxn = https://github.com/talentdeficit/jsxn.git master
dep_export_val = https://github.com/camshaft/export_val.git master

include erlang.mk
