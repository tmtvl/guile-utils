(hall-description
  (name "utils")
  (prefix "guile")
  (version "1.2.0")
  (author "Tim Van den Langenbergh")
  (copyright (2021))
  (synopsis
    "Miscellaneous utilities for GNU Guile.")
  (description
    "Various useful functions for common problems.")
  (home-page "")
  (license gpl3+)
  (dependencies `())
  (files (libraries
           ((scheme-file "utils")))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory "doc" ((texi-file "utils")))))
         (infrastructure
           ((scheme-file "guix") (scheme-file "hall")))))
