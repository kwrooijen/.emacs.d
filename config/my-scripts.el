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

(provide 'my-scripts)
