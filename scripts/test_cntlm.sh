bap ../../cntlm -d bir:cntlm.bir --primus-lisp-semantics=disable \
--a64-main-throw-errors=true \
--a64-main-prelude=$HOME/asl-interpreter/prelude.asl \
--a64-main-specs=$HOME/mra_tools/arch/regs.asl \
--a64-main-specs=$HOME/mra_tools/types.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch_instrs.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch_decode.asl \
--a64-main-specs=$HOME/asl-interpreter/tests/override.asl \
--a64-main-specs=$HOME/mra_tools/support/aes.asl \
--a64-main-specs=$HOME/mra_tools/support/barriers.asl \
--a64-main-specs=$HOME/mra_tools/support/debug.asl \
--a64-main-specs=$HOME/mra_tools/support/feature.asl \
--a64-main-specs=$HOME/mra_tools/support/hints.asl \
--a64-main-specs=$HOME/mra_tools/support/interrupts.asl \
--a64-main-specs=$HOME/mra_tools/support/memory.asl \
--a64-main-specs=$HOME/mra_tools/support/stubs.asl \
> errors.txt 2> total.txt