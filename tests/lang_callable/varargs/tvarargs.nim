discard """
  description: "Tests for varargs, in particular during overload resolution."
  disabled: true
"""

#[
TODO:
(This is a list of all the properties/scenarios to test wrt varargs, if marked
as done, then the test is written, but open questions might remain.)
- [x] non-ast varargs:
  - [x] non-ast trailing varargs:
        - [x] are implicitly converted to a `bracketExpr`
        - [x] allow for a conversion call
  - [x] non-ast non-trailing varargs
        - [x] are converted to an `argList`??? (still has open question, see below)
        - [x] allows for a conversion call
        - [x] match no more args than remaining non-defaulted/vararg formals
  - [x] subtype relationship matching
        - [x] range
        - [x] inheritance object (see: tvararg_polymorphic, t01_varargs_does_not_support_polymorphic, t07_varargs_var_subtype, t08_varargs_regular_subtype)
        - [x] inheritance ref object (see: tvararg_polymorphic)
        - [x] with converters (see: tconverter_with_varargs)
        - [x] error: widening
  - [x] overloads
        - [x] arity has precedence:
              - [x] match no-param candidate over varargs
              - [x] match one-param candidate over varargs
              - [x] match non-varargs generic over empty varargs
              - [x] match non-varargs generic over varargs (open question, see below)
              - [x] match non-generic varargs candidate over generic varargs candidate
        - [x] match openArray param candidate over varargs
        - [x] match array param candidate over varargs
        - [x] match seq param candidate over varargs
        - [x] match non-conversion candidate over with conversion (see: t03_overload_core_ambiguous_varargs)
        - [x] conversion loses to subtype
        - [x] conversion loses to widening
        - [x] conversion loses to inheritance object
        - [x] conversion loses to inheritance ref object
        - [x] conversion wins over converter
        - [x] error: varargs with/without conversions are ambiguous
- [x] generics:
      - [x] infer the type parameter
      - [x] generic parameter cannot be set to varargs (see open question below)
- [x] ast varargs:
      - [x] untyped varargs:
            - [x] override named parameter checking/act as "rest" param (see: tvarargsexpr)
            - [x] check untyped ast:
                  - [x] bracket expr/array params are not flattened  (see: tvarargsexpr)
                  - [x] constructs an nnkArgList (see: tvarargsexpr)
            - [x] error:
                  - [x] ast varargs forces comptime context
                  - [x] must only be trailing/positional
                        - [x] only one such param per routine
                  - [x] will consume following AST if "repositioned" via named param
                  - [x] disallows conversion parameter (open question, see below)
      - [x] ensure varargs[typed] param always constructs a bracket wrapper
      - [x] scenario: typed varargs param after some_type param
      - [x] typed allows for a conversion call (still has open question, see below)
      - [x] limit greediness based on trailing param count
      - [x] errors:
            - [x] ast varargs forces comptime context
            - [x] only one such param per routine
            - [x] may only be followed by non-vararg/non-defaulted params
            - [x] call arguments given for `varargs[typed]` params cannot be
                    repositioned via named argument syntax at the callsite
- [ ] varargs pragma
      - [x] errors: forces runtime context
      - [ ] ensure automatic string to cstring conversion
- [x] defensive/regression tests:
      - [x] sigmatch container re-use logic doesn't clobber existing bracket
              exprs (see: tvararg_exact_match)

FAQ:
- [x] when passing a valid call with non-ast varargs to a typed macro
        parameter, should they always become bracket expressions, or should we
        only do that with trailing varargs? At least then we'd treat them
        consistently as a form of `openArray` parameters.
      - [x] Should the handling be different between meta/non-meta routines?
              Perhaps macros would rather have non-trailing as an `argList`,
              instead of `bracket`, as each arg in the vararg is more like
              a specialized `typed[T]` at that point.
      - future direction: meta-routines should always wrap in an `arglist` and
                          non-meta-routines should always use `bracket`

- [x] should we support widening as the tests do at present?
      - the implementation is likely going to be very complex, so the plan is
        to get everything else working first, and then revisit

- [x] echo "requires", `varargs[typed, $]`, but this doesn't quite make sense:
      - [x] shouldn't type, in this case `typed`, represent the output of the
              conversion routine, `$`? an affirmative would imply:
            - no conversion functions for untyped varargs
            - we're treating `typed` as an "all", a type which is all types,
                as opposed to `any`, which is any one type, neat!
      - disallow conversions on `varargs[untyped]`
      - future direction: rework `echo` to not require `varargs[typed, $]`
      - future direction: introduce `arrayargs[T]`, or something, that does the
                          always wrapping in `bracket` that `varargs[typed]`
                          does, then `arrayargs[string, $]` can mean, take a
                          variable number of arguments, ensure they're all
                          `string` typed, via a routine `$`, and assemble an
                          array for an `openArray[string]` param. at that point
                          `var/arrayargs[T, conv]`, `T` is the final type
      - future direction: disallow conversions on `varargs[typed]`

- [x] should we be able to set a generic parameter to a varargs?
        personally, I think this is a bad idea for presently hard to fathom
        reasons, one issue I can see is that we cannot infer `varargs` and they
        seem to be a combination type + argument passing convention (syntax and
        semantics). I believe it would move us in the wrong direction, treating
        generics as templates, instead of types. Where passing in `varargs`
        would break things like the arity of a generic routine vs its instance
      - future direction: we should disallow this

- [x] should `varargs[untyped]` respect parameter passing and boundaries,
      instead of acting as a "rest of the passed argument AST"?
      - yes, it should continue doing this, as programmers would no longer be
        able to create APIs such as those found in `htmlgen`, for HTML element
        construction.

Open Questions:
- [ ] optimization: conversion calls shouldn't happen if the type is correct?
      - [ ] implies: conversion calls are side-effect free, also should they be
            anyways?

- [ ] should varargs arity precedence change based on how many matched, 0 vs 1
        vs many?
      - [x] where given a parameter `a: varargs[int]`, 0, 1, and many are lower
            than a routine with `a: int`, but 1 is higher than a generic
            routine with `a: T`, with `T` inferred as `int`.
            - yes, this should impact precedence, implementation will clarify
              if there are major issues
      - [ ] similar question wrt to varargs vs openarray across generic and
            non-generic overloads


N.B.: if an item is checked it's in the `tvarargs` test, otherwise there is a `(see: ...)` note
]#