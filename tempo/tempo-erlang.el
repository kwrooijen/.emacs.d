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


(provide 'tempo-erlang)
