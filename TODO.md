* Fix bugs:
  * Referencing a symbol that doesn't exist in a direct instruction (ldl) generates no code and no warnings. (fixed)
  * Calls to functions when the above is present causes no code to be generated for the call.
  * Offsets in call instructions to forward-defined functions, when there has been convergence in the code between the call and the function - generates the wrong offset - it's 1 byte too far.
