(require 'kmacro)

(defalias 'wrap\ string
   (kmacro "g s v g l S \" A , <escape> j"))

(provide 'my-macros)
