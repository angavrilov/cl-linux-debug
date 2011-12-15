#!/bin/bash

exec sbcl --no-sysinit --no-userinit --load ~/quicklisp/asdf.lisp --load load-libs.lisp
