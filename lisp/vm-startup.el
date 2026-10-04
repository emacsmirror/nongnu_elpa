;; This file is only here for compatibility with older VM versions  -*- lexical-binding: t; -*-
(require 'vm)
(require 'vm-macro)

;; Say so if this file's compiled form outlives the VM it was built
;; against; see `vm-assert-version' (#791).
(vm-assert-version)
(provide 'vm-startup)
