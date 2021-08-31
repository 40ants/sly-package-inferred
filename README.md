# Better support for package inferred systems for SLY

`sly-package-inferred` is an external contrib for [SLY][sly] that replaces its
completion with a function which is better suited for systems using the
package inferred style.

**Warning!** This system works with SLY version installed [from here](https://github.com/svetlyak40wt/sly/tree/patches).
Because it contains patch https://github.com/joaotavora/sly/pull/417 which is not merged yet to upstream.

## Install from MELPA

This package is not available on MELPA yet.

## Melpa-less install

Since this is an external contrib with both Elisp and Lisp parts,
merely loading the Elisp will have little effect. The contrib has to
be registered in SLY's `sly-contribs` variable for SLY to take care of
loading the Lisp side on demand.

For convenience, the `sly-package-inferred-autoloads` file takes care
of this automatically. So the following setup in your `~/.emacs` or
`~/.emacs.d/init/el` init file should be enough:

```elisp
;;; regular SLY setup
(setq inferior-lisp-program "/path/to/your/preferred/lisp")
(add-to-list 'load-path "/path/to/sly")
(require 'sly-autoloads)

(add-to-list 'load-path "/path/to/sly-package-inferred")
(require 'sly-package-inferred-autoloads)
```

In case you already have SLY loaded and running, you might have to
`M-x sly-setup` and `M-x sly-enable-contrib` to enable it.

`sly-package-inferred` should now kick in in Lisp buffers.

[sly]: https://github.com/capitaomorte/sly





