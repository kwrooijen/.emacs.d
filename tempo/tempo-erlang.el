(tempo-define-template "erlang-eunit" '(

"-module(" (substring (buffer-name) 0 -4) ")."n>
n>
"-include_lib(\"eunit/include/eunit.hrl\")."n>
n>
"%% ====================================================="n>
"%% Test Suite"n>
"%% ====================================================="n>
n>
(substring (buffer-name) 0 -4)"_test_() ->"n>
    "[?_assert(true)]."n>
n>
"%% ====================================================="n>
"%% Internal Function"n>
"%% ====================================================="n>
n>
"setup(_) ->"n>
    "ok."n>
n>
"teardown(_) ->"n>
    "ok."n>

) nil "Erlang Eunit template.")

(tempo-define-template "erlang-header" '(
"%%%==================================================================="n>
"%%%" r> n>
"%%%==================================================================="n>
) nil "Erlang header template.")


(tempo-define-template "erlang-app-src" '(
"{application, 'APPLICATIONNAME', ["n>
"    {description, \"\"},"n>
"    {vsn, \"1\"},"n>
"    {registered, []},"n>
"    {applications, ["n>
"        kernel,"n>
"        stdlib"n>
"    ]},"n>
"    {env, []}"n>
"]}."n>
) nil "Erlang app.src template.")

(tempo-define-template "erlang-rebar" '(
"{cover_enabled, true}."n>
"{cover_export_enabled, true}."n>
"{cover_print_enabled, true}."n>

"{erl_opts,["n>
"    %% {parse_transform, lager_transform},"n>
"    {src_dirs, [\"src\"]},"n>
"    warn_unused_vars,"n>
"    warn_export_all,"n>
"    warn_shadow_vars,"n>
"    warn_unused_import,"n>
"    warn_unused_function,"n>
"    warn_bif_clash,"n>
"    warn_unused_record,"n>
"    warn_deprecated_function,"n>
"    warn_obsolete_guard,"n>
"    strict_validation,"n>
"    warn_export_vars,"n>
"    warn_exported_vars,"n>
"    warn_missing_spec,"n>
"    warn_untyped_record, debug_info"n>
"]}."n>
"{deps_dir, \"deps\"}."n>
n>
"{deps, ["n>
"    %% {module, \"1.*\", {git, \"git@github.com:owner/project.git\"}, \"1.0.0\"}"n>
"]}."n>
n>
"%% {escript_name, \"MYAPPLICATION\"}."n>
"%% {escript_incl_apps, [project]}."n>
n>
) nil "Erlang rebar template.")


(provide 'tempo-erlang)

