buildapp --output sshmenu --load ~/.sbclrc --load-system sshmenu  --eval '(defun main (args) (ru.binarin.sshmenu::global-bind) (gtk:join-gtk-main))'  --entry main
