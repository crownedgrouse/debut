PROJECT = debut

ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard 
ESCRIPT_EMU_ARGS=-sname debut -pz ebin -pz deps/getops/ebin -pz deps/geas/ebin -pz deps/debris/ebin -pz deps/debbie/ebin -pz deps/debbie/deps/swab/ebin -pz deps/debbie/deps/edgar/ebin
DIALYZER_OPTS = -Wrace_conditions -Wunmatched_returns # -Wunderspecs

include lock.mk
include erlang.mk
