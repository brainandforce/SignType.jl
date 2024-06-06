using SignType
using Aqua, Test

Aqua.test_all(SignType; unbound_args = false)

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
    @testset "Real promotion" begin
        @test promote_type(Sign, Sign) === Sign
        @test promote_type(Sign, Bool) === Int
        for T in (UInt8, UInt16, UInt32, UInt64, UInt128)       # subtypes(Unsigned)
            @test promote_type(Sign, T) === signed(T)
        end
        for T in (Int8, Int16, Int32, Int64, Int128, BigInt)    # subtypes(Signed)
            @test promote_type(Sign, T) === T
        end
        for T in (BigFloat, Float16, Float32, Float64)          # subtypes(AbstractFloat)
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
