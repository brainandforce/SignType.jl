# SignType.jl

This package provides the `Sign` type, which represents the sign bit of a signed integer or float.
While the data stored by `Sign` is identical to that of `Bool`, the arithmetic semantics are defined
to match the behaviors of the real numbers `+1` and `-1`.

## Construction

A `Sign` can be constructed from a `Real` number, extracting its signbit and converting it to a
`Sign` through reinterpretation: `Sign(x::Real) = reinterpret(Sign, signbit(x))`. For any custom
numeric type that uses a different convention to represent sign, this method should be defined for
that type.

The `+` and `-` operators are also accepted as arguments, resulting in the canonical printed
representations `Sign(+)` or `Sign(-)`.

Calling `Sign` with a `Bool`, `Unsigned`, or any other type which cannot represent negative numbers
always returns `Sign(+)`. Use `reinterpret(::Type{Bool}, ::Sign)` to treat a `Sign` as a `Bool`.

## Zero elements

Zero elements are treated specially, since `Sign` cannot represent zero:
  * Calling the `Sign` constructor on a zero element returns the `Sign` that would be constructed
    for the signbit.
  * Calling `convert(Sign, x)` for a zero element `x` fails with an `InexactError`.
  * `zero(::Sign)` returns `false`.

## Arithmetic

All arithmetic operations defined on `Integer` are defined on `Sign`. Like `Bool`, addition or
subtraction of `Sign` instances (or `Sign` and `Bool` instances) results in an `Int`.

Rational division of `Sign` instances returns a `Sign`, as `Rational{Sign}` can only represent the
exact same range of values as `Sign`.

`sqrt(Sign(+))` returns `Sign(+)` instead of a `Float64`; `sqrt(Sign(-))` throws a `DomainError`
resolvable by complex conversion as usual.

## Other number systems

`Sign` construction will fail on elements of unordered fields, such as `Complex` instances.
