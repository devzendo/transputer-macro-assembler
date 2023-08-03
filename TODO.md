* Fix bugs:
  * Offsets in call instructions to nested forward-defined functions, when there has been convergence in the code between the call and the function - generates the wrong offset - it's 1 byte too far.
