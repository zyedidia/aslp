#!/bin/bash
# Lift a given stream of bytes. 
# If you encounter errors, you may need to provide additional mra_tools paths

if [ $# != 1 ]
then 
	echo "Usage: $0 bytes"
    echo "e.g. bash bap_mc_single.sh \"20 00 02 8b\""
	exit 0
fi

eval $(opam env) &&
bap-mc --show-bir --arch=aarch64 --primus-lisp-semantics=disable \
--a64-main-prelude=$HOME/asl-interpreter/prelude.asl \
--a64-main-specs=$HOME/mra_tools/arch/regs.asl \
--a64-main-specs=$HOME/mra_tools/types.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch_instrs.asl \
--a64-main-specs=$HOME/mra_tools/arch/arch_decode.asl \
-- $1
