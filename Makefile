PROJECT = aiconf
PROJECT_DESCRIPTION = conf tool for production from ailink.io
PROJECT_VERSION = 0.1.1

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DEPS = jsx ailib

dep_jsx_commit = v2.9.0
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.3.0

include erlang.mk
