#!/usr/bin/env -S guix shell -m manifest.scm -- bash

sbcl --eval '(require "asdf")' --eval '(asdf:load-system "slynk")' --eval '(slynk:create-server)'
