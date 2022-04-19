# erl-find-source for Emacs

Simple implementation of "find source at point", with no dependencies
except to `erlang-mode`, and optionally to `grep` for
`erlfs-find-callers`.

Most of the code is copied from distel by Luke Gorrie, except this
version doesn't use an erlang node.

Typical usage of this module is to bind `M-.` to
`erlfs-find-source-at-point`, and `M-*` or `M-,` to
`erlfs-find-source-unwind`, and `M-?` to `erlfs-find-callers`.

The function `erlfs-find-source-at-point` jumps to the definition of
the erlang function at point.  If the function at the point is a local
function, it jump to its defintion in the local module.  If the
function at the point is a remote call, it performs the following
algorithm:

- If a buffer exists with the name `<module>.erl`, goto that buffer.

- Otherwise, if a file `<module>.erl` exists in the current directory,
  visit that file.

- Otherwise, for each directory pattern in `erlfs-search-patterns`,
  if `<module>.erl` exists in a directory matching that pattern, visit that
  file.

- Otherwise, report that the file can't be found.

The function `erlfs-find-callers` uses `grep` to find callers of
the function under point.
