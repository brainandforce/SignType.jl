import Random: rand, rand!, AbstractRNG, SamplerType, TaskLocalRNG, Xoshiro, UnsafeView, Sampler

rand(rng::AbstractRNG, ::SamplerType{Sign})::Sign = reinterpret(Sign, rand(rng, Bool))

# This const is a broader proxy for the unexported MutableDenseArray in XoshiroSimd.jl in Random.jl
const DenseSignArrays = Union{
    DenseArray{Sign, N},
    Base.FastContiguousSubArray{UInt8,N,<:DenseArray{Sign}}
} where {N}

@static if VERSION >= v"1.12"
    # This only works properly because of JuliaLang/julia#57101
    # More complicated than the alternative version, but seems to be more canonical
    function rand!(rng::AbstractRNG, A::DenseSignArrays)
        # UnsafeView creates a writeable view that treats A as a Bool array
        # Allows writing the bits of rand!(Array{Bool}) into A without allocating new memory
        GC.@preserve A rand!(rng, UnsafeView{Bool}(pointer(A), length(A)), SamplerType{Bool}())
        return A
    end

    # The below method fixes dispatch from the non-mutating vector call to the mutating rand call
    rand!(rng::AbstractRNG, A::DenseSignArrays, sp::Sampler) = rand!(rng, A)

    function rand(rng::AbstractRNG, ::SamplerType{Sign}, dim1::Integer, extra_dims::Integer...)
        # Strange function signature to avoid method ambiguities
        A = Array{Sign}(undef, Dims(dim1, extra_dims...))
        rand!(rng, A)
        return A
    end
else
    function rand!(rng::AbstractRNG, A::DenseSignArrays)
        # This method is sus, but seems to work with no noticeable overhead
        rand!(rng, reinterpret(Bool, A))
        return A
    end
    
    # No noticeable overhead
    rand(rng::AbstractRNG, ::SamplerType{Sign}, dim1::Integer, extra_dims::Integer...) = reinterpret(Sign, rand(rng, SamplerType(Bool), dim1, extra_dims...))
end