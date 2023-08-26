# Current development activities
* Currently an assembler bug is not resetting all variables at the start of a
  convergence loop, leading to `_USER` having the value 00000430 instead of
  00000010 the first time it's used in `SZERO`. Presumably statements involving
  this variable are just being re-evaluated each time round the convergence 
  loop, leading to it being repeatedly increased wildly.
