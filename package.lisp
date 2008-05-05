
(defpackage :toolbox
  (:use :cl :anaphora :alexandria)
  (:shadow #:with-gensyms #:shuffle)
  (:nicknames :tb)
  (:export ;;; On Lisp
           #:last1 #:single #:append1 #:conc1 #:mklist #:longer
           #:filter #:group #:group-by #:flatten #:prune #:find2 #:before
           #:after #:duplicate #:split-on #:split-if #:most #:best
           #:mostn #:map-> #:mapa-b #:map1-n #:map0-n #:mappend
           #:mapcars #:rmapcar #:readlist #:prompt #:break-loop
           #:break-toplevel #:mkstr #:symb #:reread #:explode #:memoize
           ;; Function builders (functions)
           #:compose #:fif #:fint #:fun #:lrec #:ttrav #:trec
           ;; Functioin builders (macros)
           #:fn #:alrec #:on-cdrs #:atrec #:on-trees
           #:cute #:cut #:<>
           #:unforced #:delay #:force
           ;; Iteration (remove?)
           #:while #:till #:for
           #:when-bind #:when-bind*
           ;; WITH-GENSYMS, mine is better
           #:with-gensyms
           #:condlet-binds #:condlet-clause #:condlet #:if3 #:nif #:in
           #:inq #:in-if #:>casex #:>case #:do-tuples/o #:dt-args
           #:do-tuples/c #:mvdo* #:shuffle
           #:mvpsetq #:mvdo #:allf #:nilf #:tf #:toggle #:_f #:pull
           #:pull-if #:popn #:sortf #:abbrev #:abbrevs #:propmacro
           #:propmacros
           ;;; Anaphoric
           #:aif #:awhen #:awhile #:aand #:acond
           #:alambda #:ablock
           #:it #:self
           #:aif2 #:awhen2 #:awhile2 #:acond2 #:a+
           #:alist #:defanaph
           #:t-ret #:ret-t
           ;;
           #:ddfn #:defdelim #:dbind #:destruc
           #:with-matrix #:with-array #:with-struct
           #:with-places #:wplac-ex #:match #:varsym? #:binding
           #:if-match #:pat-match #:vars-in #:var? #:simple? #:gen-match
           #:match1 #:gensym? #:length-test
           ;applied ;This stuff is not basic enough, should probably be seperate
           #:=lambda #:=defun
           #:=bind #:=values #:=funcall #:=apply #:*halt* #:*default-proc*
           ;#:fork #:program #:pick-process #:most-urgent-process
           ;#:arbitrator #:wait #:yield #:setpri #:halt #:kill #:failsym
           ;#:choose #:choose-bind #:cb #:fail #:two-numbers #:defnode #:down
           ;#:cat #:jump #:compile-cmds #:up #:getr #:set-register #:setr
           ;#:pushr #:with-parses #:types #:types #:make-db #:*default-db*
           ;#:clear-db #:db-query #:db-push #:fact #:lookup #:do-answers
           ;#:compile-query #:compile-simple #:compile-and #:compile-or
           ;#:compile-not
           ;;; Misc
           #:do-file-by #:do-file-by-lines #:head #:tail
           #:unroll-circular-list
           #:by-elts #:defwrapper #:get-external-symbols #:use-package-excluding
           #:n-times #:mapcro
           #:nested-dotimes
           #:fsubvec
           ;;; FCASE
           #-clisp #:fcase
           ;;; Short-hand
           #:mvb #:mvl #:mve #:/.
           ;;; Numerics
           #:uflow->zero #:=~
           ;;; Number-theory
           #:*-mod #:expt-mod
           #:miller-rabin #:gen-prime
           #:n-digits #:get-digit #:digits<-number #:number<-digits
           #:prime?
           #:factor-trial-division #:coprime-factor-trial-division
           #:factor #:coprime-factor
           ;;; Combinatorics
           #-clisp #:!
           #:choose #:permute
           ;;; String algorithms
           #:lcs #:levenshtein-dist
           ))
