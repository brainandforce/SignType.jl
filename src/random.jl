import Random: rand, rand!, AbstractRNG, SamplerType, TaskLocalRNG, Xoshiro, UnsafeView

rand(rng::AbstractRNG, ::SamplerType{Sign})::Sign = reinterpret(Sign, rand(rng, Bool))

# This const is a broader proxy for the unexported MutableDenseArray in XoshiroSimd.jl in Random.jl
const DenseSignArrays = Union{
    DenseArray{Sign, N},
    Base.FastContiguousSubArray{UInt8,N,<:DenseArray{Sign}}
} where {N}

function rand!(rng::AbstractRNG, A::DenseSignArrays)
    # UnsafeView creates a writeable view that treats A as a Bool array
    # Allows writing the bits of rand!(Array{Bool}) into A without allocating new memory
    GC.@preserve A rand!(rng, UnsafeView{Bool}(pointer(A), length(A)), SamplerType{Bool}())
    return A
end

function rand(rng::AbstractRNG, ::SamplerType{Sign}, dim1::Integer, extra_dims::Integer...)
    # Strange function signature to avoid method ambiguities
    A = Array{Sign}(undef, Dims(dim1, extra_dims...))
    rand!(rng, A)
    return A
end