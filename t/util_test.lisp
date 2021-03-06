(in-package :cl-user)
(defpackage qi-test-util
  (:use :cl
        :qi
        :prove))
(in-package :qi-test-util)

(plan 11)

(is (qi.util::sym->str :sym)
    "sym"
    "Symbols are converted to strings.")
(is (qi.util::sym->str "sym")
    "sym"
    "Strings remain strings.")

(ok (qi.util::is-tar-url? "https://github.com/CodyReichert/qi/master/master.tar.gz") "Is a tar url.")
(ok (qi.util::is-tar-url? "https://github.com/CodyReichert/qi/master/master.tgz") "Is a tar url.")
(ok (not (qi.util::is-tar-url? "https://github.com/CodyReichert/qi/master/master.bz2")) "Is not tar url.")

(ok (qi.util::is-git-url? "https://github.com/CodyReichert/qi.git") "Is a git url.")
(ok (not (qi.util::is-git-url? "https://github.com/CodyReichert/qi")) "Is not a git url.")

(ok (qi.util::is-gh-url? "https://github.com/CodyReichert/qi") "Is a GitHub url.")
(ok (not (qi.util::is-gh-url? "https://gitlab.com/CodyReichert/qi.git")) "Is not a GitHub url.")

(ok (qi.util::is-hg-url? "https://bitbucket.org/tarballs_are_good/map-set.hg") "Is a mercurial url.")
(ok (not (qi.util::is-hg-url? "https://bitbucket.org/tarballs_are_good/map-set")) "Is not a mercurial url.")

(finalize)
