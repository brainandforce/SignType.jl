using SignType
using Aqua, Test

Aqua.test_all(SignType; unbound_args = false)

const UNSIGNED_TYPES = (UInt8, UInt16, UInt32, UInt64, UInt128)     # subtypes(Unsigned)
const SIGNED_TYPES = (Int8, Int16, Int32, Int64, Int128, BigInt)    # subtypes(Signed)
const FLOAT_TYPES = (Float16, Float32, Float64, BigFloat)           # subtypes(AbstractFloat)
const UNSIGNED_RATIONALS = map(T -> Rational{T}, UNSIGNED_TYPES)
const SIGNED_RATIONALS = map(T -> Rational{T}, SIGNED_TYPES)

@testset "All tests" begin
    @testset "Construction" begin
        # + and - literals
        @test reinterpret(Bool, Sign(+)) === false
        @test reinterpret(Bool, Sign(-)) === true
        # Unsigned integers
        @test reinterpret(Bool, Sign(0x01)) === signbit(0x01)
        @test reinterpret(Bool, Sign(0xff)) === signbit(0xff)
        # Signed integers
        @test reinterpret(Bool, Sign(+1)) === signbit(+1)
        @test reinterpret(Bool, Sign(-1)) === signbit(-1)
        @test reinterpret(Bool, Sign(big(+420))) === signbit(big(+420))
        @test reinterpret(Bool, Sign(big(-420))) === signbit(big(-420))
        # Floating point numbers
        @test reinterpret(Bool, Sign(+0.0)) === signbit(+0.0)
        @test reinterpret(Bool, Sign(-0.0)) === signbit(-0.0)
        @test reinterpret(Bool, Sign(big(+1.0))) === signbit(big(+1.0))
        @test reinterpret(Bool, Sign(big(-1.0))) === signbit(big(-1.0))
        # Rationals
        @test reinterpret(Bool, Sign(+1//2)) === signbit(+1//2)
        @test reinterpret(Bool, Sign(-1//2)) === signbit(-1//2)
    end
    @testset "Conversion to Sign" begin
        @test Bool(Sign(+)) === true
        @test_throws InexactError Bool(Sign(-))
        for T in UNSIGNED_TYPES
            @test convert(Sign, +one(T)) === Sign(+)
            @test convert(Sign, -one(T)) === Sign(+)
            # Constructing a `Sign` should always match the result of `signbit` when reinterpreted
            @test reinterpret(Bool, Sign(zero(T))) === signbit(zero(T))
            # Zero values are not representable as a Sign; conversion must fail
            @test_throws InexactError convert(Sign, zero(T))
        end
        for T in SIGNED_TYPES
            @test convert(Sign, +one(T)) === Sign(+)
            @test convert(Sign, -one(T)) === Sign(-)
            # Constructing a `Sign` should always match the result of `signbit` when reinterpreted
            @test reinterpret(Bool, Sign(zero(T))) === signbit(signbit(zero(T)))
            # Zero values are not representable as a Sign; conversion must fail
            @test_throws InexactError convert(Sign, zero(T))
        end
        for T in FLOAT_TYPES
            @test convert(Sign, +one(T)) === Sign(+)
            @test convert(Sign, -one(T)) === Sign(-)
            # Constructing a `Sign` should always match the result of `signbit` when reinterpreted
            @test reinterpret(Bool, Sign(zero(T))) === signbit(signbit(zero(T)))
            # TODO: Conversion of a zero float (which has a sign) fails
            # Should this succeed for types with a signed zero?
        end
    end
    @testset "Conversion to other types" begin
        @test Sign(Sign(+)) === Sign(+)
        @test Sign(Sign(-)) === Sign(-)
        for T in tuple(Bool, UNSIGNED_TYPES..., UNSIGNED_RATIONALS...)
            @test T(Sign(+)) === one(T)
            @test_throws InexactError T(Sign(-))
        end
        for T in tuple(SIGNED_TYPES..., SIGNED_RATIONALS..., FLOAT_TYPES...)
            @test T(Sign(+)) == +one(T)
            @test T(Sign(-)) == -one(T)
        end
        # This should create a Rational{Int}, since Rational{Sign} is redundant
        @test Rational(Sign(+)) === +1//1
        @test Rational(Sign(-)) === -1//1
        @test Integer(Sign(+)) === Sign(+)
        @test Integer(Sign(-)) === Sign(-)
    end
    @testset "Real promotion" begin
        @test promote_type(Sign, Sign) === Sign
        @test promote_type(Sign, Bool) === Int
        for T in UNSIGNED_TYPES
            @test promote_type(Sign, T) === signed(T)
        end
        for T in SIGNED_TYPES
            @test promote_type(Sign, T) === T
        end
        for T in FLOAT_TYPES
            @test promote_type(Sign, T) === T
        end
        @test promote_type(Sign, typeof(π)) === Float64
        @test promote(Sign(+), π) === (Float64(Sign(+)), Float64(π))
    end
    @testset "Complex promotion" begin
        @test promote_type(Sign, Complex{Sign}) === Complex{Sign}
        @test promote_type(Sign, Complex{Bool}) === Complex{Int}
        @test promote_type(Sign, Complex{Int16}) === Complex{Int16}
        @test promote_type(Sign, Complex{Float32}) === Complex{Float32}
        @test promote_type(Complex{Sign}, Int) === Complex{Int}
        @test promote_type(Complex{Sign}, Complex{BigFloat}) === Complex{BigFloat}
    end
    @testset "Equality and comparison" begin
        @test Sign(+) == Sign(+)
        @test Sign(-) == Sign(-)
        @test Sign(-) < Sign(+)
        @test Sign(-) <= Sign(+)
        @test Sign(-) <= Sign(-)
        @test Sign(-) < UInt8(0)
        @test Sign(+) >= UInt8(0)
        @test UInt8(0) < Sign(+)
        @test UInt8(0) >= Sign(-)
    end
    @testset "Boolean operators" begin
        # Bitwise operators
        # and
        @test Sign(+) & Sign(+) === Sign(+)
        @test Sign(+) & Sign(-) === Sign(+)
        @test Sign(-) & Sign(-) === Sign(-)
        @test Sign(-) & Sign(+) === Sign(+)
        # or
        @test Sign(+) | Sign(+) === Sign(+)
        @test Sign(+) | Sign(-) === Sign(-)
        @test Sign(-) | Sign(-) === Sign(-)
        @test Sign(-) | Sign(+) === Sign(-)
        # xor
        @test Sign(+) ⊻ Sign(+) === Sign(+)
        @test Sign(+) ⊻ Sign(-) === Sign(-)
        @test Sign(-) ⊻ Sign(-) === Sign(+)
        @test Sign(-) ⊻ Sign(+) === Sign(-)
        # not
        @test ~Sign(+) === Sign(-)
        @test ~Sign(-) === Sign(+)
        @test !Sign(+) === Sign(-)
        @test !Sign(-) === Sign(+)
        # nand
        @test Sign(+) ⊼ Sign(+) === Sign(-)
        @test Sign(+) ⊼ Sign(-) === Sign(-)
        @test Sign(-) ⊼ Sign(-) === Sign(+)
        @test Sign(-) ⊼ Sign(+) === Sign(-)
        # nor
        @test Sign(+) ⊽ Sign(+) === Sign(-)
        @test Sign(+) ⊽ Sign(-) === Sign(+)
        @test Sign(-) ⊽ Sign(-) === Sign(+)
        @test Sign(-) ⊽ Sign(+) === Sign(+)
    end
    @testset "Sign manipulation" begin
        # Negation
        @test -Sign(+) === Sign(-)
        @test -Sign(-) === Sign(+)
        # Flipping signs
        @test flipsign(Sign(+), Sign(+)) === Sign(+)
        @test flipsign(Sign(-), Sign(+)) === Sign(-)
        @test flipsign(Sign(+), Sign(-)) === Sign(-)
        @test flipsign(Sign(-), Sign(-)) === Sign(+)
        @test flipsign(Sign(+), true) === Sign(+)
        @test flipsign(Sign(-), true) === Sign(+)
        @test flipsign(Sign(+), false) === Sign(+)
        @test flipsign(Sign(-), false) === Sign(+)
        @test flipsign(Sign(+), +1) === Sign(+)
        @test flipsign(Sign(-), +1) === Sign(-)
        @test flipsign(Sign(+), -1) === Sign(-)
        @test flipsign(Sign(-), -1) === Sign(+)
        @test flipsign(Sign(+), Float16(+0.0)) === Sign(+)
        @test flipsign(Sign(-), Float16(+0.0)) === Sign(-)
        @test flipsign(Sign(+), Float16(-0.0)) === Sign(-)
        @test flipsign(Sign(-), Float16(-0.0)) === Sign(+)
        @test flipsign(Sign(+), Float32(+0.0)) === Sign(+)
        @test flipsign(Sign(-), Float32(+0.0)) === Sign(-)
        @test flipsign(Sign(+), Float32(-0.0)) === Sign(-)
        @test flipsign(Sign(-), Float32(-0.0)) === Sign(+)
        @test flipsign(Sign(+), Float64(+0.0)) === Sign(+)
        @test flipsign(Sign(-), Float64(+0.0)) === Sign(-)
        @test flipsign(Sign(+), Float64(-0.0)) === Sign(-)
        @test flipsign(Sign(-), Float64(-0.0)) === Sign(+)
        # Copying signs
        @test copysign(Sign(+), Sign(+)) === Sign(+)
        @test copysign(Sign(-), Sign(+)) === Sign(+)
        @test copysign(Sign(+), Sign(-)) === Sign(-)
        @test copysign(Sign(-), Sign(-)) === Sign(-)
        @test copysign(Sign(+), 0) === Sign(+)
        @test copysign(Sign(+), +1) === Sign(+)
        @test copysign(Sign(+), -1) === Sign(-)
        @test copysign(Sign(+), Float16(+0.0)) === Sign(+)
        @test copysign(Sign(+), Float32(-0.0)) === Sign(-)
        @test copysign(Sign(-), 0x00) === Sign(+)
        @test copysign(Sign(-), +0x01) === Sign(+)
        @test copysign(Sign(-), -0x01) === Sign(+)
        @test copysign(Sign(-), Float32(+Inf)) === Sign(+)
        @test copysign(Sign(-), Float64(-Inf)) === Sign(-)
        # TODO: should we define behavior for copysign(::Sign, NaN)?
    end
    @testset "Arithmetic" begin
        # Addition
        @test Sign(+) + Sign(+) === 2
        @test Sign(+) + Sign(-) === 0
        @test Sign(-) + Sign(+) === 0
        @test Sign(-) + Sign(-) === -2
        # Subtraction
        @test Sign(+) - Sign(+) === 0
        @test Sign(+) - Sign(-) === 2
        @test Sign(-) - Sign(+) === -2
        @test Sign(-) - Sign(-) === 0
        # Multiplication
        @test Sign(+) * Sign(+) === Sign(+)
        @test Sign(+) * Sign(-) === Sign(-)
        @test Sign(-) * Sign(+) === Sign(-)
        @test Sign(-) * Sign(-) === Sign(+)
        # Division
        @test Sign(+) / Sign(+) === Sign(+)
        @test Sign(+) / Sign(-) === Sign(-)
        @test Sign(-) / Sign(+) === Sign(-)
        @test Sign(-) / Sign(-) === Sign(+)
        @test Sign(+) // Sign(+) === Sign(+)
        @test Sign(+) // Sign(-) === Sign(-)
        @test Sign(-) // Sign(+) === Sign(-)
        @test Sign(-) // Sign(-) === Sign(+)
        @test_throws DomainError sqrt(Sign(-))
        # Exponentiation
        @test Sign(+)^0 === Sign(+)
        @test Sign(+)^1 === Sign(+)
        @test Sign(-)^0 === Sign(+)
        @test Sign(-)^1 === Sign(-)
        # Method ambiguity resolution
        @test Sign(+)^false === Sign(+)
        @test Sign(+)^true === Sign(+)
        @test Sign(-)^false === Sign(+)
        @test Sign(-)^true === Sign(-)
        @test Sign(+)^big(0) === Sign(+)
        @test Sign(+)^big(1) === Sign(+)
        @test Sign(-)^big(0) === Sign(+)
        @test Sign(-)^big(1) === Sign(-)
        # Euclidean division/remainder
        @test div(Sign(+), Sign(+)) === Sign(+)
        @test div(Sign(-), Sign(+)) === Sign(-)
        @test div(Sign(+), Sign(-)) === Sign(-)
        @test div(Sign(-), Sign(-)) === Sign(+)
        for R in (RoundUp, RoundDown, RoundNearest, RoundFromZero)
            @test div(Sign(+), Sign(+), R) === Sign(+)
            @test div(Sign(-), Sign(+), R) === Sign(-)
            @test div(Sign(+), Sign(-), R) === Sign(-)
            @test div(Sign(-), Sign(-), R) === Sign(+)
        end
        @test rem(Sign(+), Sign(+)) === false
        @test rem(Sign(-), Sign(+)) === false
        @test rem(Sign(+), Sign(-)) === false
        @test rem(Sign(-), Sign(-)) === false
    end
    @testset "Arithmetic with other types" begin
        # Int
        @test Sign(+) * +2 === +2
        @test Sign(-) * +2 === -2
        @test Sign(+) * -2 === -2
        @test Sign(-) * -2 === +2
        @test +2 * Sign(+) === +2
        @test +2 * Sign(-) === -2
        @test -2 * Sign(+) === -2
        @test -2 * Sign(-) === +2
        @test Sign(+) / +2 === inv(+2)
        @test Sign(-) / +2 === inv(-2)
        @test Sign(+) / -2 === inv(-2)
        @test Sign(-) / -2 === inv(+2)
        @test +2 / Sign(+) === +2
        @test +2 / Sign(-) === -2
        @test -2 / Sign(+) === -2
        @test -2 / Sign(-) === +2
        # Float64
        @test Sign(+) * +2.0 === +2.0
        @test Sign(-) * +2.0 === -2.0
        @test Sign(+) * -2.0 === -2.0
        @test Sign(-) * -2.0 === +2.0
        @test +2.0 * Sign(+) === +2.0
        @test +2.0 * Sign(-) === -2.0
        @test -2.0 * Sign(+) === -2.0
        @test -2.0 * Sign(-) === +2.0
        @test Sign(+) / +2.0 === inv(+2.0)
        @test Sign(-) / +2.0 === inv(-2.0)
        @test Sign(+) / -2.0 === inv(-2.0)
        @test Sign(-) / -2.0 === inv(+2.0)
        @test +2.0 / Sign(+) === +2.0
        @test +2.0 / Sign(-) === -2.0
        @test -2.0 / Sign(+) === -2.0
        @test -2.0 / Sign(-) === +2.0
        # Rational
        @test Sign(+) * +1//420 === +1//420
        @test Sign(-) * +1//420 === -1//420
        @test Sign(+) * -1//420 === -1//420
        @test Sign(-) * -1//420 === +1//420
        @test +1//420 * Sign(+) === +1//420
        @test +1//420 * Sign(-) === -1//420
        @test -1//420 * Sign(+) === -1//420
        @test -1//420 * Sign(-) === +1//420
        @test Sign(+) / +1//420 === inv(+1//420)
        @test Sign(-) / +1//420 === inv(-1//420)
        @test Sign(+) / -1//420 === inv(-1//420)
        @test Sign(-) / -1//420 === inv(+1//420)
        @test +1//420 / Sign(+) === +1//420
        @test +1//420 / Sign(-) === -1//420
        @test -1//420 / Sign(+) === -1//420
        @test -1//420 / Sign(-) === +1//420
        # Complex{Float64}
        @test Sign(+) * (+6.0 + 9.0im) === +6.0 + 9.0im
        @test Sign(-) * (+6.0 + 9.0im) === -6.0 - 9.0im
        @test Sign(+) * (-6.0 - 9.0im) === -6.0 - 9.0im
        @test Sign(-) * (-6.0 - 9.0im) === +6.0 + 9.0im
        @test (+6.0 + 9.0im) * Sign(+) === +6.0 + 9.0im
        @test (+6.0 + 9.0im) * Sign(-) === -6.0 - 9.0im
        @test (-6.0 - 9.0im) * Sign(+) === -6.0 - 9.0im
        @test (-6.0 - 9.0im) * Sign(-) === +6.0 + 9.0im
        @test Sign(+) / (+6.0 + 9.0im) === inv(+6.0 + 9.0im)
        @test Sign(-) / (+6.0 + 9.0im) === inv(-6.0 - 9.0im)
        @test Sign(+) / (-6.0 - 9.0im) === inv(-6.0 - 9.0im)
        @test Sign(-) / (-6.0 - 9.0im) === inv(+6.0 + 9.0im)
        @test (+6.0 + 9.0im) / Sign(+) === +6.0 + 9.0im
        @test (+6.0 + 9.0im) / Sign(-) === -6.0 - 9.0im
        @test (-6.0 - 9.0im) / Sign(+) === -6.0 - 9.0im
        @test (-6.0 - 9.0im) / Sign(-) === +6.0 + 9.0im
        # Complex{Bool}
        @test Sign(+) * Complex(false, false) === Complex(0, 0)
        @test Sign(+) * Complex(false, true) === Complex(0, +1)
        @test Sign(+) * Complex(true, false) === Complex(+1, 0)
        @test Sign(+) * Complex(true, true) === Complex(+1, +1)
        @test Sign(-) * Complex(false, false) === Complex(0, 0)
        @test Sign(-) * Complex(false, true) === Complex(0, -1)
        @test Sign(-) * Complex(true, false) === Complex(-1, 0)
        @test Sign(-) * Complex(true, true) === Complex(-1, -1)
        @test Complex(false, false) * Sign(+) === Complex(0, 0)
        @test Complex(false, true) * Sign(+) === Complex(0, +1)
        @test Complex(true, false) * Sign(+) === Complex(+1, 0)
        @test Complex(true, true) * Sign(+) === Complex(+1, +1)
        @test Complex(false, false) * Sign(-) === Complex(0, 0)
        @test Complex(false, true) * Sign(-) === Complex(0, -1)
        @test Complex(true, false) * Sign(-) === Complex(-1, 0)
        @test Complex(true, true) * Sign(-) === Complex(-1, -1)
        @test Sign(+) / Complex(false, false) === +inv(Complex(0, 0))
        @test Sign(+) / Complex(false, true) === +inv(Complex(0, +1))
        @test Sign(+) / Complex(true, false) === +inv(Complex(+1, 0))
        @test Sign(+) / Complex(true, true) === +inv(Complex(+1, +1))
        @test Sign(-) / Complex(false, false) === -inv(Complex(0, 0))   # NaN has a sign!
        @test Sign(-) / Complex(false, true) === -inv(Complex(0, +1))   # signed zero also
        @test Sign(-) / Complex(true, false) === -inv(Complex(+1, 0))   # causes problems here
        @test Sign(-) / Complex(true, true) === -inv(Complex(+1, +1))
        @test Complex(false, false) / Sign(+) === Complex(0, 0)
        @test Complex(false, true) / Sign(+) === Complex(0, +1)
        @test Complex(true, false) / Sign(+) === Complex(+1, 0)
        @test Complex(true, true) / Sign(+) === Complex(+1, +1)
        @test Complex(false, false) / Sign(-) === Complex(0, 0)
        @test Complex(false, true) / Sign(-) === Complex(0, -1)
        @test Complex(true, false) / Sign(-) === Complex(-1, 0)
        @test Complex(true, true) / Sign(-) === Complex(-1, -1)
        # TODO: need a test with a type subtyping Number but not Real or Complex
    end
    @testset "Properties" begin
        @test sign(Sign(+)) === Sign(+)
        @test sign(Sign(-)) === Sign(-)
        @test signbit(Sign(+)) === false
        @test signbit(Sign(-)) === true
        @test iszero(Sign(+)) === false
        @test iszero(Sign(-)) === false
        @test isone(Sign(+)) === true
        @test isone(Sign(-)) === false
        @test zero(Sign) === false
        @test one(Sign) === Sign(+)
        @test iseven(Sign(+)) === false
        @test iseven(Sign(-)) === false
        @test isodd(Sign(+)) === true
        @test isodd(Sign(-)) === true
        @test abs(Sign(+)) === Sign(+)
        @test abs(Sign(-)) === Sign(+)
        @test abs2(Sign(+)) === Sign(+)
        @test abs2(Sign(-)) === Sign(+)
        @test Base.checked_abs(Sign(+)) === Sign(+)
        @test Base.checked_abs(Sign(-)) === Sign(+)
    end
    @testset "Printed representation" begin
        @test eval(Meta.parse(repr("text/plain", Sign(+)))) === Sign(+)
        @test eval(Meta.parse(repr("text/plain", Sign(-)))) === Sign(-)
    end
end
