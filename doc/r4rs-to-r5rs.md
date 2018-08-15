http://people.csail.mit.edu/jaffer/r5rs_10.html#SEC82

Notes

Language changes

This section enumerates the changes that have been made to Scheme since the "Revised^4 report" [R4RS] was published.

The report is now a superset of the IEEE standard for Scheme [IEEEScheme]: implementations that conform to the report will also conform to the standard. This required the following changes:
The empty list is now required to count as true.
The classification of features as essential or inessential has been removed. There are now three classes of built-in procedures: primitive, library, and optional. The optional procedures are `load', `with-input-from-file', `with-output-to-file', `transcript-on', `transcript-off', and `interaction-environment', and `-' and `/' with more than two arguments. None of these are in the IEEE standard.
Programs are allowed to redefine built-in procedures. Doing so will not change the behavior of other built-in procedures.
Port has been added to the list of disjoint types.
The macro appendix has been removed. High-level macros are now part of the main body of the report. The rewrite rules for derived expressions have been replaced with macro definitions. There are no reserved identifiers.
`Syntax-rules' now allows vector patterns.
Multiple-value returns, `eval', and `dynamic-wind' have been added.
The calls that are required to be implemented in a properly tail-recursive fashion are defined explicitly.
``@'' can be used within identifiers. ``|'' is reserved for possible future extensions.
