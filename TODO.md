# Remaining Tasks
Move these into issues?

### Testing Infrastructure
  - Implement something like:
    - Given an instruction and a random initial environment
    - Run the standard evaluation for the pair
    - Run the partial evaluation and then the standard evaluation for the pair
    - Compare the final states
  - Get it running on the git repo
  - Try to come up with exhaustive list of instructions (at most 2^32)
  - How do you generate meaningful environments?

### Symbolic State
  - Perform partial evaluation using a symbolic state, rather than the current value state
  - Should map local variables to value | expression, where expressions are pure
  - (Maybe) Don't allow for the rest of the state to change, keep it immutable
  - Perform pure expression propagation during partial evaluation
  - Try to avoid non-bitvector declarations wherever possible
  - Match the evaluators nesting semantics as closely as possible
  - (Maybe) Don't delete code, leave that to dead code elimination

### Primitive Pattern Matching
  - Match the use of primitive operations to both:
    - Simplify identities, such as OR(x,0) => x
    - Convert calculations into their pure bitvector forms for BAP
  - Might be easier if we don't inline everything
  - Pattern match at a higher level, if they can be easily converted into pure bitvector expressions

### Missing Constructs
  - Change the disassembler to error out for constructs it can't handle
  - Implement missing constructs, such as:
    - For-loops, likely by just unrolling the entire thing
    - Missing lexpr cases
    - ???

### Interfacing with BAP
  - Understand the BAP plugin infrastructure and how to hook the existing disassembler call in
  - Convert a list of assignments into the form expected by BAP
  - Convert control flow modifications into the form expected by BAP
