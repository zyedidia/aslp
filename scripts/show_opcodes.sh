#!/bin/bash
# Tests all opcodes for given instruction

if [ $# != 1 ] && [ $# != 3 ]
then 
	echo "Usage: $0 instruction [in] [out]"
    echo "e.g. bash bap_mc_coverage.sh aarch64_integer_arithmetic_add_sub_carry result.txt errors.txt"
	exit 0
fi

eval $(opam env)

# Get all the opcodes for the given instruction
OPCODES=$(echo ":opcodes A64 $1" | \
~/asl-interpreter/asli prelude.asl $HOME/mra_tools/arch/regs.asl \
$HOME/mra_tools/types.asl $HOME/mra_tools/arch/arch.asl \
$HOME/mra_tools/arch/arch_instrs.asl $HOME/mra_tools/arch/arch_decode.asl \
$HOME/mra_tools/support/aes.asl $HOME/mra_tools/support/barriers.asl \
$HOME/mra_tools/support/debug.asl $HOME/mra_tools/support/feature.asl \
$HOME/mra_tools/support/hints.asl $HOME/mra_tools/support/interrupts.asl \
$HOME/mra_tools/support/memory.asl $HOME/mra_tools/support/stubs.asl \
$HOME/mra_tools/support/fetchdecode.asl)

echo $OPCODES
