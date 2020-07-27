(in-package 'compiler)
(and (boundp 'compiler::*cc*) ;;skcl

     (if (search "gcc" *cc*) (setq *cc* "gcc -B/usr/local/src/gcc-1.18/ -I."))
     (load "opts.lsp")) 
