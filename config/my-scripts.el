(defun my-up-to-script (file command buff)
    "Script for executing a command when a certain file is found"
    (async-shell-command (format "
#!/bin/bash

while [ $PWD != \"/\" ]; do
    if [ -f %s ]; then
        %s
        exit 0
    fi
    cd ..
done
" file command) buff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Elixir ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq my-elixir-guard-init "
#!/bin/bash

echo 'group :development do
  gem \"guard-elixir\"
end' > Gemfile

echo 'notification :emacs
guard :elixir do
  watch(%r{^test/(.*)_test\.exs})
  watch(%r{^lib/(.+)\.ex$})           { |m| \"test/#{m[1]}_test.exs\" }
  watch(%r{^test/test_helper.exs$})   { \"test\" }
end' > Guardfile
")

(provide 'my-scripts)
