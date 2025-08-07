# reformulas 0.4.1

* add `sparse` argument to `mkReTrms` (for controlling sparsity of effects model matrix J, in Z construction)

# reformulas 0.4.0 (2024-11-03)

* `expandAllGrpVars` etc. expands complex terms
* `anySpecials` now handles "naked" specials (e.g. `s` rather than `s(...)`) properly
* `findbars` now only looks on the RHS of a formula (restore back-compatibility in cases where a term with `|` occurs on the LHS, as in the `tramME` package)
* add tests (`tinytest`)
* fix `noSpecials` bug (complex LHS and empty RHS after eliminating specials)

# reformulas 0.3.0 (2024-06-05)

* Preparing for `lme4` inclusion: include/move functions from `lme4` (`expandDoubleVerts` etc.), new imports/exports, etc.

# reformulas 0.2.0 (2024-03-13)

Initial release
