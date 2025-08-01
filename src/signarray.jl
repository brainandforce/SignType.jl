# This shortens lines. A lot.
const UI = UndefInitializer

"""
    SignArray{N} <: AbstractArray{Sign,N}

Space-efficient `N`-dimensional array of signs, using one bit to store each sign.

# Implementation

This type wraps a [`BitArray{N}`](@ref BitArray) and reinterprets the `Bool` objects as `Sign` 
objects. The same caveats for operating on [`BitArray`](@ref) (concurrent operations involving any 
writes are *not* thread safe!) apply to this type.
"""
struct SignArray{N} <: AbstractArray{Sign,N}
    data::BitArray{N}
    function SignArray{N}(::UI, dims::Vararg{Int,N}) where N
        return new(BitArray{N}(undef, dims...))
    end
end

const SignVector = SignArray{1}
const SignMatrix = SignArray{2}

SignArray{N}(::UI, dims::Vararg{Integer,N}) where N = SignArray{N}(undef, map(Int, dims)...)
SignArray(::UI, dims::Vararg{Integer,N}) where N = SignArray{N}(undef, dims...)
SignArray{N}(::UI, dims::Tuple{Vararg{Integer,N}}) where N = SignArray(undef, dims...)
SignArray(::UI, dims::Tuple{Vararg{Integer,N}}) where N = SignArray{N}(undef, dims...)

Base.size(s::SignArray) = size(s.data)
Base.getindex(s::SignArray, i...) = reinterpret(Sign, s.data[i...])
Base.setindex!(s::SignArray, x, i...) = setindex!(s.data, reinterpret(Bool, convert(Sign, x)), i...)

#---similar----------------------------------------------------------------------------------------#

Base.similar(s::SignArray) = SignArray(undef, size(s))

Base.similar(::SignArray, dims::Int...) = SignArray(undef, dims...)
Base.similar(::SignArray, dims::Dims) = SignArray(undef, dims...)

Base.similar(::SignArray, ::Type{Sign}, dims::Dims) = SignArray(undef, dims...)

# Interoperability with BitArray
Base.similar(::SignArray, ::Type{Bool}, dims::Dims) = BitArray(undef, dims...)
Base.similar(::BitArray, ::Type{Sign}, dims::Dims) = SignArray(undef, dims...)

#---Passthrough methods that work with the underlying BitArray-------------------------------------#

Base.fill!(s::SignArray, x) = (fill!(s.data, reinterpret(Bool, convert(Sign, x))); return s)

#---Postiives and negatives------------------------------------------------------------------------#
"""
    positives(dims)

Creates a `SignArray` of the specified dimensions with all entries positive.
"""
positives(dims::Vararg{Union{Integer,OneTo}}) = fill!(SignArray(undef, map(last, dims)...), Sign(+))

"""
    negatives(dims)

Creates a `SignArray` of the specified dimensions with all entries negative.
"""
negatives(dims::Vararg{Union{Integer,OneTo}}) = fill!(SignArray(undef, map(last, dims)...), Sign(-))
