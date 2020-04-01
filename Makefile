PROJECT = aiconf
PROJECT_DESCRIPTION = conf tool for production from ailink.io
PROJECT_VERSION = 0.1.6

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DEPS = jiffy ailib

dep_jiffy_commit = 1.0.1
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git v0.4.3

include erlang.mk
