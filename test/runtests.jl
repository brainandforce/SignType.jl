using SignType
using Aqua, Test

Aqua.test_all(SignType; unbound_args = false)

const UNSIGNED_TYPES = (UInt8, UInt16, UInt32, UInt64, UInt128)     # subtypes(Unsigned)
const SIGNED_TYPES = (Int8, Int16, Int32, Int64, Int128, BigInt)    # subtypes(Signed)
const FLOAT_TYPES = (BigFloat, Float16, Float32, Float64)           # subtypes(AbstractFloat)
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
            # Zero is signed in floats, so construction should always succeed here
            @test convert(Sign, +zero(T)) === Sign(+)
            @test convert(Sign, -zero(T)) === Sign(+)
        end
    end
    @testset "Conversion to other types" begin
        @test Sign(Sign(+)) === Sign(+)
        @test Sign(Sign(-)) === Sign(-)
        @test Bool(Sign(+)) === true
        @test_throws InexactError Bool(Sign(-))
        for T in UNSIGNED_TYPES
            @test T(Sign(+)) === one(T)
            @test_throws InexactError T(Sign(-))
        end
        for T in SIGNED_TYPES
            @test T(Sign(+)) === +one(T)
            @test T(Sign(-)) === -one(T)
        end
        for T in FLOAT_TYPES
            @test T(Sign(+)) === +one(T)
            @test T(Sign(-)) === -one(T)
        end
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
    @testset "Arithmetic" begin
        @test -Sign(+) === Sign(-)
        @test -Sign(-) === Sign(+)
        @test Sign(+) + Sign(+) === 2
        @test Sign(+) + Sign(-) === 0
        @test Sign(-) + Sign(+) === 0
        @test Sign(-) + Sign(-) === -2
        @test Sign(+) - Sign(+) === 0
        @test Sign(+) - Sign(-) === 2
        @test Sign(-) - Sign(+) === -2
        @test Sign(-) - Sign(-) === 0
        @test Sign(+) * Sign(+) === Sign(+)
        @test Sign(+) * Sign(-) === Sign(-)
        @test Sign(-) * Sign(+) === Sign(-)
        @test Sign(-) * Sign(-) === Sign(+)
        @test Sign(+) / Sign(+) === Sign(+)
        @test Sign(+) / Sign(-) === Sign(-)
        @test Sign(-) / Sign(+) === Sign(-)
        @test Sign(-) / Sign(-) === Sign(+)
        @test Sign(+) // Sign(+) === Sign(+)
        @test Sign(+) // Sign(-) === Sign(-)
        @test Sign(-) // Sign(+) === Sign(-)
        @test Sign(-) // Sign(-) === Sign(+)
        @test_throws DomainError sqrt(Sign(-))
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
    end
    @testset "Printed representation" begin
        @test eval(Meta.parse(repr("text/plain", Sign(+)))) === Sign(+)
        @test eval(Meta.parse(repr("text/plain", Sign(-)))) === Sign(-)
    end
end
