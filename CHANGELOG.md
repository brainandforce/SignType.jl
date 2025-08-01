# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1] - 2025-08-01

### Fixed
  * Invalid promotion rules (credit to @nsajko for pointing this out: https://discourse.julialang.org/t/ann-signtype-jl-what-if-bool-but-sign/131182/4?u=brainandforce)

## [0.1.0] - 2025-07-26

### Added
  * The `Sign` type
  * The `Sign(+)` and `Sign(-)` literals
  * Arithmetic semantics for signs
  * Boolean semantics for signs (matching `Bool`)
  * Promotion and conversion to and from other number types
  * Random generation of signs

[0.1.1]: https://github.com/brainandforce/SignType.jl/releases/tag/v0.1.1
[0.1.0]: https://github.com/brainandforce/SignType.jl/releases/tag/v0.1.0
[Unreleased]: https://github.com/brainandforce/SignType.jl

