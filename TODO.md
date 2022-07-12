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
    - Extract them from real binaries for now
  - How do you generate meaningful environments?

### Partial Evaluation Simplification
  - Simplify the partial evaluator to make reasoning about its correctness easier
  - Clear assertions/exceptions for cases that aren't supported
  - Make global parts of the state immutable, only track changes to local variables
  - Consistent renaming of local variables to avoid naming collisions
  - Revert all changes to 'eval.ml' and 'value.ml'
  - Match the evaluators structure as closely as possible:
    - Same recursive function layout
    - Same children access order
    - Same local variable nesting approach
  - Don't delete any declaration or assignments, leave deletion to dead code elimination

### Symbolic State
  - Perform partial evaluation using a symbolic state, rather than the current value state
  - Should map local variables to value | expression, where expressions are pure
  - Perform pure expression propagation during partial evaluation
  - Try to avoid non-bitvector declarations wherever possible

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
    - Short-circuit operators
    - LExpr Fun Calls
    - Stmt Fun Calls
    - Partial Update Lexprs
    - ???

### Interfacing with BAP
  - Understand the BAP plugin infrastructure and how to hook the existing disassembler call in
    - Idea seems to be that you make a promise to the BAP knowledge base to provide semantics information after disassembly
    - See 'plugins//x86/x86_legacy_bil_lifter.ml' for an example
  - Convert a list of assignments into the form expected by BAP
    - Main issue is difference in value semantics, due to the lack of integer operations in BIL
