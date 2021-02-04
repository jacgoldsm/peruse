# Version 0.1.0

* Initial Release

* Added a `NEWS.md` file to track changes to the package.

# Version 0.2.0

* Will be released soon as Version 0.2.0

* Removed `as_Iterator()`

* Changed `is_Iterator()` to use `inherits()` instead of `class() ==`

* Fixed bug in `we_have()` that wrote over an old set-created `Iterator` when a new one was made, even with a different name

* Removed `hash_df` data structure

* Removed dependency on {R6}

* Added `yield_more()`

* Added `move_next()` and `move_more()`

* Added `current()`

* Added `yield_while()`

# Version 0.3.0

* Changed the `Iterator` to use an `environment` as a backend instead of a `list`, and
added `clone()`

* Added a well-defined search path for `Iterator`s

* Added seed arguments for reproducibility to `yield_more()`, `yield_while()`, and `move_more()`

* Made error messages more clear for `Iterator`

* Added `move_while()`

* Added access to the current iteration number with `.iter`

* Completely changed the `move_*()` backend to use `yield_*()`
