# Version 0.1.0

* Initial Release

* Added a `NEWS.md` file to track changes to the package.

# Version 0.2.0

* Will be released soon as Version 0.2.0

* Removed `as_Iterator()`

* Changes `is_Iterator()` to use `inherits()` instead of `class() ==`

* Fixed bug in `we_have()` that wrote over an old set-created `Iterator` when a new one was made, even with a different name

* Removed `hash_df` data structure

* Removed dependency on {R6}

* Added `yield_more()`

* Added `move_next()` and `move_more()`

* Added `current()`

* Added `yield_while()`
