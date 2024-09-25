module SignType

import Base: promote_rule, convert
import Base: <, iseven, isodd, abs, abs2
import Base: &, |, ~, !, xor
import Base: flipsign, copysign, +, -, *, /, //, ^, div, rem
import Base: iszero, isone, zero, one
import Base: show

"""
    Sign <: Signed

A type representing the sign of a number. This is simply a `Bool` with its arithmetic semantics
corresponding to the sign bit conventions of `Signed` or `AbstractFloat`. In a sense, this type can
be thought of as `Int1`, with no bits representing digits.

# Construction

`Sign` objects can be constructed from `Real` numbers, returning `reinterpret(Sign, signbit(x))`.
Note that for inputs that correspond to zero, this returns `Sign(+)` for any integer stored with a
twos' complement representation. With the exception of zero elements, the behavior matches
`Base.sign`.

For other types `T<:Number`, `Sign(::T)` is only defined if `T` represents an element of an ordered
field, allowing for well-defined comparisons of elements. `Sign(z::Complex)` will throw an
`InexactError` unless `imag(z)` is zero.

Special constructors that utilize the functions `+` and `-` as literal inputs are defined, so that
`Sign(+)` and `Sign(-)` behave as one would expect.

# Conversion

Conversion is identical to construction, with one important exception: converting a zero element
to a `Sign` always fails with an `InexactError`, even though constructing it always succeeds 
(defaulting to the the sign associated with that element):
```julia-repl
julia> Sign(-0.0)
Sign(-)

julia> convert(Sign, 0.0)
ERROR: InexactError: convert(Sign, -0.0)
Stacktrace:
...
```
To treat a `Sign` `s` as a `Bool`, use `reinterpret(Bool, s)`. Construction or conversion uses
arithmetic semantics, so `Bool(Sign(+)) === true` and `Bool(Sign(-))` throws an `InexactError`.

# Promotion

In general, `Sign` promotes in the following manner:
  * `Sign` and `T<:Integer` promote to `signed(T)`.
  * `Sign` and `Bool` promote to `Int`.
  * `Sign` and `AbstractIrrational` promote to `Float64`, as negation of an `AbstractIrrational`
    results in a `Float64` by default.
  * `Sign` and `T<:Real` promote to `T` by default. This assumes `T` can represent negative numbers;
    a promote rule must be defined if this is not the case.

# Arithmetic

Standard arithmetic operations (`+`, `-`, `*`, `/`) are supported. Addition and subtraction of
`Sign` objects with each other or `Bool` produce an `Int` result, analogous to the addition or
subtraction of `Bool` instances.

The rational division operator `//` between two `Sign` objects returns a `Sign`, not a
`Rational{Sign}`, as the two types represent the exact same set of values, rendering 
`Rational{Sign}` redundant.

The square root of `Sign(+)` returns `Sign(+)`, not a `Float64` as other square root operations do.
The square root of `Sign(-)` throws a `DomainError`; use `sqrt(complex(Sign(-)))`.

!!! danger
    Because zero is not representable by a `Sign` type, `zero(Sign)` returns `false` as a strong
    zero, which is a `Bool`, not a `Sign`!

# Logical operators

Logical operators on `Sign` instances have identical semantics to `Bool` in terms of their bitwise
representations: `Sign(+)` acts like `false`, and `Sign(-)` acts like `true`. All logical operators
supported on `Bool` are also supported on `Sign`.
"""
primitive type Sign <: Signed 8 end

# constructors with + and - literals
Sign(::typeof(+)) = reinterpret(Sign, false)
Sign(::typeof(-)) = reinterpret(Sign, true)

# Obtain signs from real numbers
Sign(x::T) where T<:Real = reinterpret(Sign, signbit(x))
# Needed to resolve method ambiguities
Sign(x::Sign) = x
Sign(x::Rational) = reinterpret(Sign, signbit(x))
Sign(x::BigInt) = reinterpret(Sign, signbit(x))
Sign(x::BigFloat) = reinterpret(Sign, signbit(x))

#---Pretty printing--------------------------------------------------------------------------------#

show(io::IO, s::Sign) = print(io, Sign, '(', reinterpret(Bool, s) ? '-' : '+', ')')

#---Constructors and conversion for other primitive numeric types----------------------------------#

Base.Bool(s::Sign) = Bool(ifelse(reinterpret(Bool, s), -1, 1))

Base.UInt8(s::Sign) = UInt8(ifelse(reinterpret(Bool, s), -1, 1))
Base.UInt16(s::Sign) = UInt16(ifelse(reinterpret(Bool, s), -1, 1))
Base.UInt32(s::Sign) = UInt32(ifelse(reinterpret(Bool, s), -1, 1))
Base.UInt64(s::Sign) = UInt64(ifelse(reinterpret(Bool, s), -1, 1))
Base.UInt128(s::Sign) = UInt128(ifelse(reinterpret(Bool, s), -1, 1))

Base.Int8(s::Sign) = Int8(ifelse(reinterpret(Bool, s), -1, 1))
Base.Int16(s::Sign) = Int16(ifelse(reinterpret(Bool, s), -1, 1))
Base.Int32(s::Sign) = Int32(ifelse(reinterpret(Bool, s), -1, 1))
Base.Int64(s::Sign) = Int64(ifelse(reinterpret(Bool, s), -1, 1))
Base.Int128(s::Sign) = Int128(ifelse(reinterpret(Bool, s), -1, 1))
Base.BigInt(s::Sign) = BigInt(ifelse(reinterpret(Bool, s), -1, 1))

Base.Integer(s::Sign) = s

Base.Rational{T}(s::Sign) where T<:Integer = Rational{T}(ifelse(reinterpret(Bool, s), -1, 1))
# There's no point in making a Rational{Sign}, so just assume Rational{Int} is desired
Base.Rational(s::Sign) = Rational{Int}(ifelse(reinterpret(Bool, s), -1, 1))

Base.Float16(s::Sign) = Float16(ifelse(reinterpret(Bool, s), -1, 1))
Base.Float32(s::Sign) = Float32(ifelse(reinterpret(Bool, s), -1, 1))
Base.Float64(s::Sign) = Float64(ifelse(reinterpret(Bool, s), -1, 1))
Base.BigFloat(s::Sign) = BigFloat(ifelse(reinterpret(Bool, s), -1, 1))

(::Type{T})(s::Sign) where T<:Real = T(ifelse(reinterpret(Bool, s), -1, 1))

convert(::Type{Sign}, x::Real) = iszero(x) ? throw(InexactError(:convert, Sign, x)) : Sign(x)

#---Promotion rules--------------------------------------------------------------------------------#

promote_rule(::Type{Sign}, ::Type{T}) where T<:Integer = signed(T)
promote_rule(::Type{Sign}, ::Type{T}) where T<:AbstractIrrational = Float64
promote_rule(::Type{Sign}, ::Type{T}) where T<:Real = T
# TODO: Since Rational{Sign} is redundant, just convert the result to Sign
# promote_rule(::Type{Sign}, ::Type{Rational{Sign}}) === Sign

#---Equality, comparison, and properties-----------------------------------------------------------#

<(x::Sign, y::Sign) = (x == Sign(-) && y == Sign(+))

# Avoid promotion in the unsigned case
<(x::Sign, ::Union{Bool,Unsigned}) = (x === Sign(-))
<(x::Union{Bool,Unsigned}, y::Sign) = iszero(x) && y === Sign(+)

iseven(::Sign) = false
isodd(::Sign) = true

abs(::Sign) = Sign(+)
abs2(::Sign) = Sign(+)

Base.checked_abs(::Sign) = Sign(+)

#---Boolean operators------------------------------------------------------------------------------#

for f in (:~, :!)
    @eval $f(x::Sign) = reinterpret(Sign, $f(reinterpret(Bool, x)))
end

for f in (:&, :|, :xor)
    @eval $f(x::Sign, y::Sign) = reinterpret(Sign, $f(reinterpret(Bool, x), reinterpret(Bool, y)))
end

#---Arithmetic-------------------------------------------------------------------------------------#

-(x::Sign) = ~x

flipsign(x::Sign, y::Sign) = xor(x, y)
flipsign(::Sign, ::Bool) = Sign(+)
flipsign(x::Sign, y::Float16) = flipsign(x, Sign(y))
flipsign(x::Sign, y::Float32) = flipsign(x, Sign(y))
flipsign(x::Sign, y::Float64) = flipsign(x, Sign(y))
flipsign(x::Sign, y::Real) =  flipsign(x, Sign(y))
# The default implementation of this is not type-stable
flipsign(x::Complex{Bool}, y::Sign) = flipsign(convert(Complex{Int}, x), y)

copysign(::Sign, y::Real) = Sign(y)
# Needed to resolve method ambiguities
copysign(::Sign, y::Signed) = Sign(y)
copysign(::Sign, y::Float16) = Sign(y)
copysign(::Sign, y::Float32) = Sign(y)
copysign(::Sign, y::Float64) = Sign(y)

+(x::Sign, y::Sign) = +(Int(x), Int(y))
-(x::Sign, y::Sign) = -(Int(x), Int(y))
*(x::Sign, y::Sign) = xor(x, y)
/(x::Sign, y::Sign) = *(x, y)
# There is no reason to construct Rational{Sign}; it is equivalent to `Sign`
//(x::Sign, y::Sign) = *(x, y)
# Exponentiation with integers is type-stable
^(s::Sign, n::Integer) = reinterpret(Sign, reinterpret(Bool, s) && isodd(n))
# Needed to resolve method ambiguities
^(s::Sign, b::Bool) = reinterpret(Sign, reinterpret(Bool, s) && b)
^(s::Sign, n::BigInt) = reinterpret(Sign, reinterpret(Bool, s) && isodd(n))

div(x::Sign, y::Sign) = *(x, y)
# Needed to resolve method ambiguities
div(x::Sign, y::Sign, ::RoundingMode{:FromZero}) = *(x, y)
div(x::Sign, y::Sign, ::RoundingMode{:Nearest}) = *(x, y)
div(x::Sign, y::Sign, ::RoundingMode{:Down}) = *(x, y)
div(x::Sign, y::Sign, ::RoundingMode{:Up}) = *(x, y)
rem(::Sign, ::Sign) = false

# Avoid type promotion in some multiplication operations
*(x::Sign, y::Number) = flipsign(y, x)
*(x::Number, y::Sign) = flipsign(x, y)
# Needed to resolve method ambiguities
*(x::Sign, y::Real) = flipsign(y, x)
*(x::Real, y::Sign) = flipsign(x, y)
*(x::Sign, y::Integer) = flipsign(y, x)
*(x::Integer, y::Sign) = flipsign(x, y)
*(x::Sign, y::Rational) = flipsign(y, x)
*(x::Rational, y::Sign) = flipsign(x, y)

*(x::Sign, y::Complex) = flipsign(y, x)
*(x::Complex, y::Sign) = flipsign(x, y)
*(x::Sign, y::Complex{Bool}) = flipsign(y, x)
*(x::Complex{Bool}, y::Sign) = flipsign(x, y)

# Same, but with division
/(x::Number, y::Sign) = flipsign(x, y)
/(x::Sign, y::Number) = flipsign(inv(y), x)
# Needed to resolve method ambiguities
/(x::Rational, y::Sign) = flipsign(x, y)
/(x::Sign, y::Rational) = flipsign(inv(y), x)
/(x::Complex, y::Sign) = flipsign(x, y)
/(x::Sign, y::Complex) = flipsign(inv(y), x)

#---zero() and one()-------------------------------------------------------------------------------#

# IMPORTANT: zero(Sign) cannot be of type Sign!
iszero(::Sign) = false
isone(s::Sign) = !reinterpret(Bool, s)

zero(::Union{Sign,Type{Sign}}) = false
one(::Union{Sign,Type{Sign}}) = Sign(+)

#---Construction from other ordered fields---------------------------------------------------------#
#=
"""
    SignType.OrderedField{B}
    SignType.OrderedField(T::Type)
    SignType.OrderedField(x)

A trait type where the type parameter `B` is `true` if the
"""
struct OrderedField{B}
    OrderedField{B}() where B = (@assert B isa Bool "Type parameter must be a Bool."; new())
end

const IsOrderedField = OrderedField{true}
const IsNotOrderedField = OrderedField{false}

"""
    OrderedFieldError <: Exception

The input type is not part of an ordered field, which means that no sign can be determined for it.
"""
struct OrderedFieldError <: Exception
end
=#

export Sign

end
