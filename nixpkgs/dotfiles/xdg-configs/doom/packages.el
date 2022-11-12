;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-nACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! spotify)
(package! terminal-here)
(package! imenu-list)

(package! emmet-mode)
(package! evil-matchit)
(package! mu4e-alert)
(package! js-doc)
(package! emojify)
(package! rainbow-mode)
(package! slack)
(package! nyan-mode)
(package! csv-mode)
(package! org-reveal)
(package! ox-gfm)
(package! ox-hugo)
(package! magit-section)
(package! eyebrowse)
(package! atomic-chrome)
(package! golden-ratio)
(package! sublimity)

;; Themes
(package! badwolf-theme)
(package! humanoid-themes)
(package! kaolin-themes)
(package! nyx-theme)

(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! kubernetes)
(package! kubernetes-evil)

(package! tree-sitter)
(package! tree-sitter-langs)

(package! json-mode :disable t)
(package! jsonian
  :recipe (:host github :repo "iwahbe/jsonian"))

;; If this was not enough, we also have: https://github.com/DarthFennec/highlight-indent-guides
(package! hightlight-indentation
  :recipe (:host github :repo "antonj/Highlight-Indentation-for-Emacs"))

(package! pulsing-cursor
    :recipe (:host github :repo "jasonjckn/pulsing-cursor"))

(package! minimap)
(package! google-translate)
;; (package! indium)
;; (package! darkroom)
;; (package! org-present)
;; (package! mini-modeline)
;; (package! smeargle)
;; (package! magit-delta)

;; (package! exwm
;;   :recipe (:host github :repo "ch11ng/exwm"))

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
