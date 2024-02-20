bits(64) CNTPS_CTL_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '0' then
            if SCR_EL3.EEL2 == '1' then
                UNDEFINED;
            elsif SCR_EL3.ST == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return Zeros(32):CNTPS_CTL_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTPS_CTL_EL1;
                  

bits(64) ERXMISC2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC2_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC2_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXMISC2_EL1;
                  

bits(64) VBAR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return VBAR_EL2;
    elsif PSTATE.EL == EL3 then
        return VBAR_EL2;
                  

bits(64) VBAR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.VBAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x250];
        else
            return VBAR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return VBAR_EL2;
        else
            return VBAR_EL1;
    elsif PSTATE.EL == EL3 then
        return VBAR_EL1;
                  

bits(64) CNTHPS_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return Zeros(32):CNTHPS_TVAL_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return Zeros(32):CNTHPS_TVAL_EL2;
                  

bits(64) CNTHPS_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_TVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_TVAL_EL2;
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_TVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_TVAL_EL2;
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTP_TVAL_EL0;
                  

bits(64) ICC_BPR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_BPR1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_BPR1_EL1;
            else
                return Zeros(32):ICC_BPR1_EL1;
        else
            return Zeros(32):ICC_BPR1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_BPR1_EL1;
            else
                return Zeros(32):ICC_BPR1_EL1;
        else
            return Zeros(32):ICC_BPR1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_BPR1_EL1;
            else
                return Zeros(32):ICC_BPR1_EL1;
                  

bits(64) ACTLR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return ACTLR_EL2;
    elsif PSTATE.EL == EL3 then
        return ACTLR_EL2;
                  

bits(64) AMCGCR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCGCR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCGCR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCGCR_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCGCR_EL0;
                  

bits(64) AMCNTENCLR1_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENCLR1_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENCLR1_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENCLR1_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCNTENCLR1_EL0;
                  

bits(64) ID_ISAR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR1_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR1_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR1_EL1;
                  

bits(64) DBGWVR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGWVRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGWVR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGWVR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGWVR_EL1[UInt(CRm[3:0])];
                  

bits(64) CPACR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CPACR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x100];
        else
            return Zeros(32):CPACR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return Zeros(32):CPTR_EL2;
        else
            return Zeros(32):CPACR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CPACR_EL1;
                  

bits(64) CPACR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x100];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return Zeros(32):CPACR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CPACR_EL1;
        else
            UNDEFINED;
                  

bits(64) CNTKCTL_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        return Zeros(32):CNTKCTL_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTHCTL_EL2;
        else
            return Zeros(32):CNTKCTL_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTKCTL_EL1;
                  

bits(64) CNTKCTL_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTKCTL_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CNTKCTL_EL1;
        else
            UNDEFINED;
                  

bits(64) APDBKeyHi_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APDBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDBKeyHi_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDBKeyHi_EL1;
    elsif PSTATE.EL == EL3 then
        return APDBKeyHi_EL1;
                  

bits(64) MVFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):MVFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):MVFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MVFR1_EL1;
                  

bits(64) ACTLR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return ACTLR_EL3;
                  

bits(64) ICC_SRE_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && ICC_SRE_EL2.Enable == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && ICC_SRE_EL3.Enable == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_SRE_EL1;
            else
                return Zeros(32):ICC_SRE_EL1;
        else
            return Zeros(32):ICC_SRE_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && ICC_SRE_EL3.Enable == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_SRE_EL1;
            else
                return Zeros(32):ICC_SRE_EL1;
        else
            return Zeros(32):ICC_SRE_EL1;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            return Zeros(32):ICC_SRE_EL1;
        else
            return Zeros(32):ICC_SRE_EL1;
                  

bits(64) MPAMVPM1_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x948];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM1_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM1_EL2;
                  

bits(64) PMOVSSET_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMOVSSET_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMOVSSET_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMOVSSET_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMOVSSET_EL0;
                  

bits(64) PMCEID1_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCEID1_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCEID1_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCEID1_EL0;
    elsif PSTATE.EL == EL3 then
        return PMCEID1_EL0;
                  

bits(64) DBGWCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGWCRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGWCR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGWCR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGWCR_EL1[UInt(CRm[3:0])];
                  

bits(64) MPAMIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAMIDR_EL1.HAS_HCR == '1' && MPAMHCR_EL2.TRAP_MPAMIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAMIDR_EL1.HAS_TIDR == '1' && MPAM2_EL2.TIDR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return MPAMIDR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return MPAMIDR_EL1;
                  

bits(64) SPSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x160];
        else
            return Zeros(32):SPSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):SPSR_EL2;
        else
            return Zeros(32):SPSR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_EL1;
                  

bits(64) SPSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x160];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):SPSR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):SPSR_EL1;
        else
            UNDEFINED;
                  

bits(64) SPSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return Zeros(32):SPSR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):SPSR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_EL2;
                  

bits(64) ELR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x230];
        else
            return ELR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return ELR_EL2;
        else
            return ELR_EL1;
    elsif PSTATE.EL == EL3 then
        return ELR_EL1;
                  

bits(64) ELR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x230];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return ELR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return ELR_EL1;
        else
            UNDEFINED;
                  

bits(64) ELR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return ELR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return ELR_EL2;
    elsif PSTATE.EL == EL3 then
        return ELR_EL2;
                  

bits(64) ICV_IAR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_IAR1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR1_EL1;
                  

bits(64) VBAR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return VBAR_EL3;
                  

bits(64) HDFGRTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1D0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return HDFGRTR_EL2;
    elsif PSTATE.EL == EL3 then
        return HDFGRTR_EL2;
                  

bits(64) ID_PFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_PFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_PFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_PFR1_EL1;
                  

bits(64) VBAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.VBAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x250];
        else
            return VBAR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return VBAR_EL2;
        else
            return VBAR_EL1;
    elsif PSTATE.EL == EL3 then
        return VBAR_EL1;
                  

bits(64) VBAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x250];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return VBAR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return VBAR_EL1;
        else
            UNDEFINED;
                  

bits(64) PMBPTR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMBPTR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.E2PB == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x810];
        else
            return PMBPTR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMBPTR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMBPTR_EL1;
                  

bits(64) ID_MMFR4_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!IsZero(ID_MMFR4_EL1) || boolean IMPLEMENTATION_DEFINED "ID_MMFR4_EL1 trapped by HCR_EL2.TID3") && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_MMFR4_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_MMFR4_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_MMFR4_EL1;
                  

bits(64) AMCNTENSET0_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENSET0_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENSET0_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENSET0_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCNTENSET0_EL0;
                  

bits(64) ELR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return ELR_EL3;
                  

bits(64) SPSR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_EL3;
                  

bits(64) ID_AA64MMFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64MMFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64MMFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64MMFR0_EL1;
                  

bits(64) SCR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return SCR_EL3;
                  

bits(64) MDRAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDRA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MDRAR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MDRAR_EL1;
    elsif PSTATE.EL == EL3 then
        return MDRAR_EL1;
                  

bits(64) CNTVCT_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VCTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTHCTL_EL2.EL1TVCT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTVCT_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTHCTL_EL2.EL1TVCT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTVCT_EL0;
    elsif PSTATE.EL == EL2 then
        return CNTVCT_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTVCT_EL0;
                  

bits(64) ID_AA64AFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64AFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64AFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64AFR1_EL1;
                  

bits(64) ID_AA64DFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64DFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64DFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64DFR0_EL1;
                  

bits(64) ICC_SRE_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ICC_SRE_EL3;
                  

bits(64) ACTLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TACR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x118];
        else
            return ACTLR_EL1;
    elsif PSTATE.EL == EL2 then
        return ACTLR_EL1;
    elsif PSTATE.EL == EL3 then
        return ACTLR_EL1;
                  

bits(64) CNTV_TVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_TVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_TVAL_EL2;
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_TVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_TVAL_EL2;
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTV_TVAL_EL0;
                  

bits(64) CNTV_TVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTV_TVAL_EL0;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CNTV_TVAL_EL0;
        else
            UNDEFINED;
                  

bits(64) ID_AA64ZFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!IsZero(ID_AA64ZFR0_EL1) || boolean IMPLEMENTATION_DEFINED "ID_AA64ZFR0_EL1 trapped by HCR_EL2.TID3") && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64ZFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64ZFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64ZFR0_EL1;
                  

bits(64) MDCCSR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && MDSCR_EL1.TDCC == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (HCR_EL2.TGE == '1' || MDCR_EL2.[TDE,TDA] != '00') then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDCCSR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDCCSR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDCCSR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MDCCSR_EL0;
                  

bits(64) ICH_ELRSR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ICH_ELRSR_EL2;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICH_ELRSR_EL2;
                  

bits(64) ICC_SRE_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && ICC_SRE_EL3.Enable == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_SRE_EL2;
    elsif PSTATE.EL == EL3 then
        if !EL2Enabled() then
            UNDEFINED;
        else
            return Zeros(32):ICC_SRE_EL2;
                  

bits(64) PMSFCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSFCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSFCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSFCR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSFCR_EL1;
                  

bits(64) VDISR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x500];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return VDISR_EL2;
    elsif PSTATE.EL == EL3 then
        return VDISR_EL2;
                  

bits(64) VDISR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.AMO == '1' then
            return VDISR_EL2;
        else
            return DISR_EL1;
    elsif PSTATE.EL == EL2 then
        return DISR_EL1;
    elsif PSTATE.EL == EL3 then
        return DISR_EL1;
                  

bits(64) HPFAR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return HPFAR_EL2;
    elsif PSTATE.EL == EL3 then
        return HPFAR_EL2;
                  

bits(64) SPSR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return Zeros(32):SPSR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):SPSR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_EL2;
                  

bits(64) SPSR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x160];
        else
            return Zeros(32):SPSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):SPSR_EL2;
        else
            return Zeros(32):SPSR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_EL1;
                  

bits(64) PMSELR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.[ER,EN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSELR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMSELR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSELR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMSELR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMSELR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMSELR_EL0;
                  

bits(64) ELR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return ELR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return ELR_EL2;
    elsif PSTATE.EL == EL3 then
        return ELR_EL2;
                  

bits(64) ELR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x230];
        else
            return ELR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return ELR_EL2;
        else
            return ELR_EL1;
    elsif PSTATE.EL == EL3 then
        return ELR_EL1;
                  

bits(64) LORC_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.LORC_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORC_EL1;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORC_EL1;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            return LORC_EL1;
                  

bits(64) LOREA_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.LOREA_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LOREA_EL1;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LOREA_EL1;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            return LOREA_EL1;
                  

bits(64) ID_AA64MMFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64MMFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64MMFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64MMFR1_EL1;
                  

bits(64) ID_AA64AFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64AFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64AFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64AFR0_EL1;
                  

bits(64) ERRIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERRIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERRIDR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERRIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return ERRIDR_EL1;
                  

bits(64) SCXTNUM_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.SCXTNUM_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x188];
        else
            return SCXTNUM_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return SCXTNUM_EL2;
        else
            return SCXTNUM_EL1;
    elsif PSTATE.EL == EL3 then
        return SCXTNUM_EL1;
                  

bits(64) SCXTNUM_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x188];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return SCXTNUM_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return SCXTNUM_EL1;
        else
            UNDEFINED;
                  

bits(64) TPIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x090];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return TPIDR_EL2;
    elsif PSTATE.EL == EL3 then
        return TPIDR_EL2;
                  

bits(64) ID_PFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_PFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_PFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_PFR0_EL1;
                  

bits(64) OSLSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.OSLSR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDOSA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSLSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSLSR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):OSLSR_EL1;
                  

bits(64) AMCNTENSET1_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENSET1_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENSET1_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENSET1_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCNTENSET1_EL0;
                  

bits(64) ID_MMFR5_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!IsZero(ID_MMFR5_EL1) || boolean IMPLEMENTATION_DEFINED "ID_MMFR5_EL1 trapped by HCR_EL2.TID3") && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_MMFR5_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_MMFR5_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_MMFR5_EL1;
                  

bits(64) ICV_AP1R_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return ICV_AP1R_EL1[UInt(op2[1:0])];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return ICC_AP1R_EL1[UInt(op2[1:0])];
            else
                return ICC_AP1R_EL1[UInt(op2[1:0])];
        else
            return ICC_AP1R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return ICC_AP1R_EL1[UInt(op2[1:0])];
            else
                return ICC_AP1R_EL1[UInt(op2[1:0])];
        else
            return ICC_AP1R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return ICC_AP1R_EL1[UInt(op2[1:0])];
            else
                return ICC_AP1R_EL1[UInt(op2[1:0])];
                  

bits(64) DBGDTRRX_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if Halted() then
        return Zeros(32):DBGDTRRX_EL0;
    elsif PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && MDSCR_EL1.TDCC == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (HCR_EL2.TGE == '1' || MDCR_EL2.[TDE,TDA] != '00') then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGDTRRX_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGDTRRX_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGDTRRX_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DBGDTRRX_EL0;
                  

bits(64) AMAIR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return AMAIR_EL2;
    elsif PSTATE.EL == EL3 then
        return AMAIR_EL2;
                  

bits(64) AMAIR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AMAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x148];
        else
            return AMAIR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return AMAIR_EL2;
        else
            return AMAIR_EL1;
    elsif PSTATE.EL == EL3 then
        return AMAIR_EL1;
                  

bits(64) ID_AA64DFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64DFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64DFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64DFR1_EL1;
                  

bits(64) CNTV_CTL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_CTL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_CTL_EL2;
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x170];
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_CTL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_CTL_EL2;
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTV_CTL_EL0;
                  

bits(64) CNTV_CTL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVVCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                return NVMem[0x170];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTV_CTL_EL0;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CNTV_CTL_EL0;
        else
            UNDEFINED;
                  

bits(64) ESR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return Zeros(32):ESR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ESR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ESR_EL2;
                  

bits(64) ESR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ESR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x138];
        else
            return Zeros(32):ESR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):ESR_EL2;
        else
            return Zeros(32):ESR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ESR_EL1;
                  

bits(64) PMEVTYPER_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])];
    elsif PSTATE.EL == EL3 then
        return PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])];
                  

bits(64) ESR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ESR_EL3;
                  

bits(64) AMAIR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return AMAIR_EL3;
                  

bits(64) RGSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):RGSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):RGSR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):RGSR_EL1;
                  

bits(64) ICH_VTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ICH_VTR_EL2;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICH_VTR_EL2;
                  

bits(64) SCXTNUM_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && SCTLR_EL1.TSCXT == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] != '11' && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.SCXTNUM_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCTLR_EL2.TSCXT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return SCXTNUM_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.SCXTNUM_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return SCXTNUM_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return SCXTNUM_EL0;
    elsif PSTATE.EL == EL3 then
        return SCXTNUM_EL0;
                  

bits(64) TPIDR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return TPIDR_EL3;
                  

bits(64) GMID_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID5 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return GMID_EL1;
    elsif PSTATE.EL == EL2 then
        return GMID_EL1;
    elsif PSTATE.EL == EL3 then
        return GMID_EL1;
                  

bits(64) PMSLATFR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSLATFR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x848];
        else
            return PMSLATFR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSLATFR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSLATFR_EL1;
                  

bits(64) PMCNTENCLR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCNTENCLR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCNTENCLR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCNTENCLR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMCNTENCLR_EL0;
                  

bits(64) TPIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TPIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return TPIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return TPIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return TPIDR_EL1;
                  

bits(64) ERXMISC3_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC3_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC3_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXMISC3_EL1;
                  

bits(64) SCXTNUM_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return SCXTNUM_EL2;
    elsif PSTATE.EL == EL3 then
        return SCXTNUM_EL2;
                  

bits(64) SCXTNUM_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.SCXTNUM_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x188];
        else
            return SCXTNUM_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return SCXTNUM_EL2;
        else
            return SCXTNUM_EL1;
    elsif PSTATE.EL == EL3 then
        return SCXTNUM_EL1;
                  

bits(64) OSDTRTX_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSDTRTX_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSDTRTX_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):OSDTRTX_EL1;
                  

bits(64) PMSEVFR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSEVFR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x830];
        else
            return PMSEVFR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSEVFR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSEVFR_EL1;
                  

bits(64) CNTHV_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CNTHV_CTL_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTHV_CTL_EL2;
                  

bits(64) CNTHV_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_CTL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_CTL_EL2;
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x170];
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_CTL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_CTL_EL2;
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTV_CTL_EL0;
                  

bits(64) VMPIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x050];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return VMPIDR_EL2;
    elsif PSTATE.EL == EL3 then
        if !HaveEL(EL2) then
            return MPIDR_EL1;
        else
            return VMPIDR_EL2;
                  

bits(64) VMPIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.MPIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) then
            return VMPIDR_EL2;
        else
            return MPIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return MPIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return MPIDR_EL1;
                  

bits(64) ID_ISAR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR0_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR0_EL1;
                  

bits(64) DBGPRCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGPRCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDOSA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGPRCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGPRCR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DBGPRCR_EL1;
                  

bits(64) AMAIR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AMAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x148];
        else
            return AMAIR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return AMAIR_EL2;
        else
            return AMAIR_EL1;
    elsif PSTATE.EL == EL3 then
        return AMAIR_EL1;
                  

bits(64) AMAIR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x148];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return AMAIR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return AMAIR_EL1;
        else
            UNDEFINED;
                  

bits(64) ICC_BPR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_BPR0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_BPR0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_BPR0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_BPR0_EL1;
                  

bits(64) ESR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ESR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x138];
        else
            return Zeros(32):ESR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):ESR_EL2;
        else
            return Zeros(32):ESR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ESR_EL1;
                  

bits(64) ESR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x138];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):ESR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):ESR_EL1;
        else
            UNDEFINED;
                  

bits(64) ESR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return Zeros(32):ESR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ESR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ESR_EL2;
                  

bits(64) AMCNTENCLR0_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENCLR0_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HAFGRTR_EL2.AMCNTEN0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENCLR0_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCNTENCLR0_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCNTENCLR0_EL0;
                  

bits(64) APIBKeyLo_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APIBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIBKeyLo_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIBKeyLo_EL1;
    elsif PSTATE.EL == EL3 then
        return APIBKeyLo_EL1;
                  

bits(64) MVFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):MVFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):MVFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MVFR0_EL1;
                  

bits(64) SSBS_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        return Zeros(51):PSTATE.SSBS:Zeros(12);
    elsif PSTATE.EL == EL1 then
        return Zeros(51):PSTATE.SSBS:Zeros(12);
    elsif PSTATE.EL == EL2 then
        return Zeros(51):PSTATE.SSBS:Zeros(12);
    elsif PSTATE.EL == EL3 then
        return Zeros(51):PSTATE.SSBS:Zeros(12);
                  

bits(64) MPAMVPM0_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x940];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM0_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM0_EL2;
                  

bits(64) CNTPCT_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PCTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTPCT_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CNTHCTL_EL2.EL1PCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTPCT_EL0;
    elsif PSTATE.EL == EL2 then
        return CNTPCT_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTPCT_EL0;
                  

bits(64) MPAMHCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x930];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMHCR_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMHCR_EL2;
                  

bits(64) CNTP_TVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_TVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_TVAL_EL2;
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_TVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_TVAL_EL2;
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTP_TVAL_EL0;
                  

bits(64) CNTP_TVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTP_TVAL_EL0;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CNTP_TVAL_EL0;
        else
            UNDEFINED;
                  

bits(64) ICC_AP1R_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return ICV_AP1R_EL1[UInt(op2[1:0])];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return ICC_AP1R_EL1[UInt(op2[1:0])];
            else
                return ICC_AP1R_EL1[UInt(op2[1:0])];
        else
            return ICC_AP1R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return ICC_AP1R_EL1[UInt(op2[1:0])];
            else
                return ICC_AP1R_EL1[UInt(op2[1:0])];
        else
            return ICC_AP1R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return ICC_AP1R_EL1[UInt(op2[1:0])];
            else
                return ICC_AP1R_EL1[UInt(op2[1:0])];
                  

bits(64) TPIDR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TPIDR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return TPIDR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TPIDR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return TPIDR_EL0;
    elsif PSTATE.EL == EL2 then
        return TPIDR_EL0;
    elsif PSTATE.EL == EL3 then
        return TPIDR_EL0;
                  

bits(64) SCXTNUM_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return SCXTNUM_EL3;
                  

bits(64) ICV_IAR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_IAR0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR0_EL1;
                  

bits(64) PMBLIMITR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMBLIMITR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.E2PB == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x800];
        else
            return PMBLIMITR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMBLIMITR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMBLIMITR_EL1;
                  

bits(64) PMCEID0_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCEID0_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCEID0_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCEID0_EL0;
    elsif PSTATE.EL == EL3 then
        return PMCEID0_EL0;
                  

bits(64) ID_AA64PFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64PFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64PFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64PFR1_EL1;
                  

bits(64) DSPSR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if !Halted() then
        UNDEFINED;
    else
        return Zeros(32):DSPSR_EL0;
                  

bits(64) CCSIDR2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID4 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CCSIDR2_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CCSIDR2_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CCSIDR2_EL1;
                  

bits(64) MAIR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.MAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x140];
        else
            return MAIR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return MAIR_EL2;
        else
            return MAIR_EL1;
    elsif PSTATE.EL == EL3 then
        return MAIR_EL1;
                  

bits(64) MAIR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x140];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return MAIR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return MAIR_EL1;
        else
            UNDEFINED;
                  

bits(64) GCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return GCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return GCR_EL1;
    elsif PSTATE.EL == EL3 then
        return GCR_EL1;
                  

bits(64) DLR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if !Halted() then
        UNDEFINED;
    else
        return DLR_EL0;
                  

bits(64) ID_DFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_DFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_DFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_DFR0_EL1;
                  

bits(64) ICV_IGRPEN0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_IGRPEN0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN0_EL1;
                  

bits(64) CCSIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID4 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CCSIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CCSIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CCSIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CCSIDR_EL1;
                  

bits(64) ID_PFR2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_PFR2_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_PFR2_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_PFR2_EL1;
                  

bits(64) ICC_IGRPEN0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_IGRPEN0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN0_EL1;
                  

bits(64) RVBAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL1 && IsHighestEL(EL1) then
        return RVBAR_EL1;
    else
        UNDEFINED;
                  

bits(64) APIBKeyHi_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APIBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIBKeyHi_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIBKeyHi_EL1;
    elsif PSTATE.EL == EL3 then
        return APIBKeyHi_EL1;
                  

bits(64) AMUSERENR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMUSERENR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMUSERENR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMUSERENR_EL0;
    elsif PSTATE.EL == EL3 then
        return AMUSERENR_EL0;
                  

bits(64) ICC_IAR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_IAR0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR0_EL1;
                  

bits(64) ICC_HPPIR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_HPPIR1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR1_EL1;
                  

bits(64) OSDTRRX_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSDTRRX_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSDTRRX_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):OSDTRRX_EL1;
                  

bits(64) ID_ISAR2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR2_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR2_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR2_EL1;
                  

bits(64) MAIR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return MAIR_EL2;
    elsif PSTATE.EL == EL3 then
        return MAIR_EL2;
                  

bits(64) MAIR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.MAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x140];
        else
            return MAIR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return MAIR_EL2;
        else
            return MAIR_EL1;
    elsif PSTATE.EL == EL3 then
        return MAIR_EL1;
                  

bits(64) DBGCLAIMCLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGCLAIM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGCLAIMCLR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGCLAIMCLR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DBGCLAIMCLR_EL1;
                  

bits(64) PMXEVTYPER_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMXEVTYPER_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMXEVTYPER_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMXEVTYPER_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMXEVTYPER_EL0;
                  

bits(64) RVBAR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL3 && IsHighestEL(EL3) then
        return RVBAR_EL3;
    else
        UNDEFINED;
                  

bits(64) DIT_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        return Zeros(39):PSTATE.DIT:Zeros(24);
    elsif PSTATE.EL == EL1 then
        return Zeros(39):PSTATE.DIT:Zeros(24);
    elsif PSTATE.EL == EL2 then
        return Zeros(39):PSTATE.DIT:Zeros(24);
    elsif PSTATE.EL == EL3 then
        return Zeros(39):PSTATE.DIT:Zeros(24);
                  

bits(64) CNTHP_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return CNTHP_CVAL_EL2;
    elsif PSTATE.EL == EL3 then
        return CNTHP_CVAL_EL2;
                  

bits(64) CNTHP_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHPS_CVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return CNTHP_CVAL_EL2;
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x178];
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHPS_CVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return CNTHP_CVAL_EL2;
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTP_CVAL_EL0;
                  

bits(64) ERXMISC1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC1_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC1_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXMISC1_EL1;
                  

bits(64) PMINTENCLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMINTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMINTENCLR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMINTENCLR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMINTENCLR_EL1;
                  

bits(64) RVBAR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL1 && EL2Enabled() && IsHighestEL(EL2) && HCR_EL2.NV == '1' then
        AArch64.SystemAccessTrap(EL2, 0x18);
    elsif PSTATE.EL == EL2 && IsHighestEL(EL2) then
        return RVBAR_EL2;
    else
        UNDEFINED;
                  

bits(64) AMCG1IDR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCG1IDR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCG1IDR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCG1IDR_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCG1IDR_EL0;
                  

bits(64) ICV_BPR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_BPR0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_BPR0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_BPR0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_BPR0_EL1;
                  

bits(64) CNTPS_TVAL_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '0' then
            if SCR_EL3.EEL2 == '1' then
                UNDEFINED;
            elsif SCR_EL3.ST == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return Zeros(32):CNTPS_TVAL_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTPS_TVAL_EL1;
                  

bits(64) AMEVTYPER0_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMEVTYPER0_EL0[UInt(CRm[0]:op2[2:0])];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMEVTYPER0_EL0[UInt(CRm[0]:op2[2:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMEVTYPER0_EL0[UInt(CRm[0]:op2[2:0])];
    elsif PSTATE.EL == EL3 then
        return AMEVTYPER0_EL0[UInt(CRm[0]:op2[2:0])];
                  

bits(64) MPAMVPM2_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x950];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM2_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM2_EL2;
                  

bits(64) NZCV_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        return Zeros(32):PSTATE.[N,Z,C,V]:Zeros(28);
    elsif PSTATE.EL == EL1 then
        return Zeros(32):PSTATE.[N,Z,C,V]:Zeros(28);
    elsif PSTATE.EL == EL2 then
        return Zeros(32):PSTATE.[N,Z,C,V]:Zeros(28);
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PSTATE.[N,Z,C,V]:Zeros(28);
                  

bits(64) MAIR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return MAIR_EL3;
                  

bits(64) SPSR_und_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):SPSR_und;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_und;
                  

bits(64) MVFR2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):MVFR2_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):MVFR2_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MVFR2_EL1;
                  

bits(64) CNTHVS_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return CNTHVS_CVAL_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return CNTHVS_CVAL_EL2;
                  

bits(64) CNTHVS_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHVS_CVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return CNTHV_CVAL_EL2;
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x168];
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHVS_CVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return CNTHV_CVAL_EL2;
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTV_CVAL_EL0;
                  

bits(64) ICV_AP0R_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return ICV_AP0R_EL1[UInt(op2[1:0])];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICC_AP0R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICC_AP0R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICC_AP0R_EL1[UInt(op2[1:0])];
                  

bits(64) AMEVCNTVOFF0_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0xA00+8*UInt(CRm[0]:op2[2:0])];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.AMVOFFEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMEVCNTVOFF0_EL2[UInt(CRm[0]:op2[2:0])];
    elsif PSTATE.EL == EL3 then
        return AMEVCNTVOFF0_EL2[UInt(CRm[0]:op2[2:0])];
                  

bits(64) ICC_IAR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_IAR1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IAR1_EL1;
                  

bits(64) ISR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ISR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ISR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ISR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ISR_EL1;
                  

bits(64) ICC_HPPIR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_HPPIR0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR0_EL1;
                  

bits(64) AMEVCNTVOFF1_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0xA80+8*UInt(CRm[0]:op2[2:0])];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.AMVOFFEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMEVCNTVOFF1_EL2[UInt(CRm[0]:op2[2:0])];
    elsif PSTATE.EL == EL3 then
        return AMEVCNTVOFF1_EL2[UInt(CRm[0]:op2[2:0])];
                  

bits(64) TCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x120];
        else
            return TCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TCR_EL2;
        else
            return TCR_EL1;
    elsif PSTATE.EL == EL3 then
        return TCR_EL1;
                  

bits(64) TCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x120];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TCR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return TCR_EL1;
        else
            UNDEFINED;
                  

bits(64) ID_ISAR3_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR3_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR3_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR3_EL1;
                  

bits(64) ERXMISC0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC0_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXMISC0_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXMISC0_EL1;
                  

bits(64) ICC_IGRPEN1_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_IGRPEN1_EL3;
                  

bits(64) MPAMVPMV_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x938];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPMV_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPMV_EL2;
                  

bits(64) TCO_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        return Zeros(38):PSTATE.TCO:Zeros(25);
    elsif PSTATE.EL == EL1 then
        return Zeros(38):PSTATE.TCO:Zeros(25);
    elsif PSTATE.EL == EL2 then
        return Zeros(38):PSTATE.TCO:Zeros(25);
    elsif PSTATE.EL == EL3 then
        return Zeros(38):PSTATE.TCO:Zeros(25);
                  

bits(64) ICV_BPR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_BPR1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_BPR1_EL1;
            else
                return Zeros(32):ICC_BPR1_EL1;
        else
            return Zeros(32):ICC_BPR1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_BPR1_EL1;
            else
                return Zeros(32):ICC_BPR1_EL1;
        else
            return Zeros(32):ICC_BPR1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_BPR1_EL1;
            else
                return Zeros(32):ICC_BPR1_EL1;
                  

bits(64) CNTP_CTL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_CTL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_CTL_EL2;
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x180];
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_CTL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_CTL_EL2;
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTP_CTL_EL0;
                  

bits(64) CNTP_CTL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVPCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                return NVMem[0x180];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTP_CTL_EL0;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CNTP_CTL_EL0;
        else
            UNDEFINED;
                  

bits(64) DAIF_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && ((EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') || SCTLR_EL1.UMA == '0') then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            return Zeros(54):PSTATE.[D,A,I,F]:Zeros(6);
    elsif PSTATE.EL == EL1 then
        return Zeros(54):PSTATE.[D,A,I,F]:Zeros(6);
    elsif PSTATE.EL == EL2 then
        return Zeros(54):PSTATE.[D,A,I,F]:Zeros(6);
    elsif PSTATE.EL == EL3 then
        return Zeros(54):PSTATE.[D,A,I,F]:Zeros(6);
                  

bits(64) PMCCFILTR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCCFILTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCCFILTR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCCFILTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCCFILTR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCCFILTR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMCCFILTR_EL0;
                  

bits(64) MPAMVPM3_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x958];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM3_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM3_EL2;
                  

bits(64) DCZID_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.DCZID_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):DCZID_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.DCZID_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):DCZID_EL0;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):DCZID_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DCZID_EL0;
                  

bits(64) HACR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):HACR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):HACR_EL2;
                  

bits(64) CTR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && SCTLR_EL1.UCT == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] != '11' && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCTLR_EL2.UCT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CTR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CTR_EL0;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CTR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CTR_EL0;
                  

bits(64) ID_AFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_AFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_AFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_AFR0_EL1;
                  

bits(64) PAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.PAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return PAR_EL1;
    elsif PSTATE.EL == EL2 then
        return PAR_EL1;
    elsif PSTATE.EL == EL3 then
        return PAR_EL1;
                  

bits(64) MPIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.MPIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) then
            return VMPIDR_EL2;
        else
            return MPIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return MPIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return MPIDR_EL1;
                  

bits(64) TCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return TCR_EL2;
    elsif PSTATE.EL == EL3 then
        return TCR_EL2;
                  

bits(64) TCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x120];
        else
            return TCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TCR_EL2;
        else
            return TCR_EL1;
    elsif PSTATE.EL == EL3 then
        return TCR_EL1;
                  

bits(64) AIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):AIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):AIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AIDR_EL1;
                  

bits(64) ID_AA64PFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64PFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64PFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64PFR0_EL1;
                  

bits(64) APDBKeyLo_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APDBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDBKeyLo_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDBKeyLo_EL1;
    elsif PSTATE.EL == EL3 then
        return APDBKeyLo_EL1;
                  

bits(64) ICV_IGRPEN1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_IGRPEN1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_IGRPEN1_EL1;
            else
                return Zeros(32):ICC_IGRPEN1_EL1;
        else
            return Zeros(32):ICC_IGRPEN1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_IGRPEN1_EL1;
            else
                return Zeros(32):ICC_IGRPEN1_EL1;
        else
            return Zeros(32):ICC_IGRPEN1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_IGRPEN1_EL1;
            else
                return Zeros(32):ICC_IGRPEN1_EL1;
                  

bits(64) VTCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x040];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):VTCR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):VTCR_EL2;
                  

bits(64) OSECCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.OSECCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSECCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSECCR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):OSECCR_EL1;
                  

bits(64) ID_AA64MMFR2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!IsZero(ID_AA64MMFR2_EL1) || boolean IMPLEMENTATION_DEFINED "ID_AA64MMFR2 trapped by HCR_EL2.TID3") && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64MMFR2_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64MMFR2_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64MMFR2_EL1;
                  

bits(64) ID_DFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!IsZero(ID_DFR1_EL1) || boolean IMPLEMENTATION_DEFINED "ID_DFR1 trapped by HCR_EL2.TID3") && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_DFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_DFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_DFR1_EL1;
                  

bits(64) CNTHP_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CNTHP_CTL_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTHP_CTL_EL2;
                  

bits(64) CNTHP_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_CTL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_CTL_EL2;
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x180];
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_CTL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_CTL_EL2;
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTP_CTL_EL0;
                  

bits(64) ICC_AP0R_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return ICV_AP0R_EL1[UInt(op2[1:0])];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICC_AP0R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICC_AP0R_EL1[UInt(op2[1:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICC_AP0R_EL1[UInt(op2[1:0])];
                  

bits(64) DBGDTR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if Halted() then
        return DBGDTR_EL0;
    elsif PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && MDSCR_EL1.TDCC == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (HCR_EL2.TGE == '1' || MDCR_EL2.[TDE,TDA] != '00') then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return DBGDTR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return DBGDTR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return DBGDTR_EL0;
    elsif PSTATE.EL == EL3 then
        return DBGDTR_EL0;
                  

bits(64) ICC_IGRPEN1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_IGRPEN1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_IGRPEN1_EL1;
            else
                return Zeros(32):ICC_IGRPEN1_EL1;
        else
            return Zeros(32):ICC_IGRPEN1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_IGRPEN1_EL1;
            else
                return Zeros(32):ICC_IGRPEN1_EL1;
        else
            return Zeros(32):ICC_IGRPEN1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_IGRPEN1_EL1;
            else
                return Zeros(32):ICC_IGRPEN1_EL1;
                  

bits(64) CNTPOFF_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1A8];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ECVEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return CNTPOFF_EL2;
    elsif PSTATE.EL == EL3 then
        return CNTPOFF_EL2;
                  

bits(64) HFGRTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1B8];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return HFGRTR_EL2;
    elsif PSTATE.EL == EL3 then
        return HFGRTR_EL2;
                  

bits(64) TCR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):TCR_EL3;
                  

bits(64) FPSR_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CPACR_EL1.FPEN != '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x00);
            else
                AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CPTR_EL2.FPEN != '11' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPSR;
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPSR;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPSR;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPSR;
                  

bits(64) ERXPFGCDN_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FIEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXPFGCDN_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXPFGCDN_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXPFGCDN_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXPFGCDN_EL1;
                  

bits(64) PMSIRR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSIRR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x840];
        else
            return PMSIRR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSIRR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSIRR_EL1;
                  

bits(64) CNTHV_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return CNTHV_CVAL_EL2;
    elsif PSTATE.EL == EL3 then
        return CNTHV_CVAL_EL2;
                  

bits(64) CNTHV_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHVS_CVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return CNTHV_CVAL_EL2;
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x168];
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHVS_CVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return CNTHV_CVAL_EL2;
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTV_CVAL_EL0;
                  

bits(64) TTBR0_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return TTBR0_EL2;
    elsif PSTATE.EL == EL3 then
        return TTBR0_EL2;
                  

bits(64) TTBR0_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TTBR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x200];
        else
            return TTBR0_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TTBR0_EL2;
        else
            return TTBR0_EL1;
    elsif PSTATE.EL == EL3 then
        return TTBR0_EL1;
                  

bits(64) LORSA_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.LORSA_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORSA_EL1;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORSA_EL1;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            return LORSA_EL1;
                  

bits(64) ICC_CTLR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_CTLR_EL3;
                  

bits(64) TRFCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TTRF == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x880];
        else
            return TRFCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return TRFCR_EL2;
        else
            return TRFCR_EL1;
    elsif PSTATE.EL == EL3 then
        return TRFCR_EL1;
                  

bits(64) TRFCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x880];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return TRFCR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return TRFCR_EL1;
        else
            UNDEFINED;
                  

bits(64) PMSICR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSICR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x838];
        else
            return PMSICR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSICR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSICR_EL1;
                  

bits(64) SCTLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.SCTLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x110];
        else
            return SCTLR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return SCTLR_EL2;
        else
            return SCTLR_EL1;
    elsif PSTATE.EL == EL3 then
        return SCTLR_EL1;
                  

bits(64) SCTLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x110];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return SCTLR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return SCTLR_EL1;
        else
            UNDEFINED;
                  

bits(64) MPAM1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAM2_EL2.TRAPMPAM1EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x900];
        else
            return MPAM1_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return MPAM2_EL2;
        else
            return MPAM1_EL1;
    elsif PSTATE.EL == EL3 then
        return MPAM1_EL1;
                  

bits(64) MPAM1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x900];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return MPAM1_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return MPAM1_EL1;
        else
            UNDEFINED;
                  

bits(64) ICH_HCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x4C0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ICH_HCR_EL2;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICH_HCR_EL2;
                  

bits(64) IFSR32_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):IFSR32_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):IFSR32_EL2;
                  

bits(64) APIAKeyHi_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APIAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIAKeyHi_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIAKeyHi_EL1;
    elsif PSTATE.EL == EL3 then
        return APIAKeyHi_EL1;
                  

bits(64) CPTR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CPTR_EL3;
                  

bits(64) ICH_VMCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x4C8];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ICH_VMCR_EL2;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICH_VMCR_EL2;
                  

bits(64) PMMIR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMMIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMMIR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMMIR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMMIR_EL1;
                  

bits(64) ERXFR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXFR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXFR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXFR_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXFR_EL1;
                  

bits(64) CPTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):CPTR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CPTR_EL2;
                  

bits(64) CPTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CPACR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x100];
        else
            return Zeros(32):CPACR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return Zeros(32):CPTR_EL2;
        else
            return Zeros(32):CPACR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CPACR_EL1;
                  

bits(64) CNTHPS_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return CNTHPS_CVAL_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return CNTHPS_CVAL_EL2;
                  

bits(64) CNTHPS_CVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHPS_CVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return CNTHP_CVAL_EL2;
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x178];
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHPS_CVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return CNTHP_CVAL_EL2;
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTP_CVAL_EL0;
                  

bits(64) VSTTBR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x030];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return VSTTBR_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return VSTTBR_EL2;
                  

bits(64) HSTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x080];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):HSTR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):HSTR_EL2;
                  

bits(64) APGAKeyLo_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APGAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APGAKeyLo_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APGAKeyLo_EL1;
    elsif PSTATE.EL == EL3 then
        return APGAKeyLo_EL1;
                  

bits(64) SPSR_irq_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):SPSR_irq;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_irq;
                  

bits(64) ID_MMFR3_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_MMFR3_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_MMFR3_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_MMFR3_EL1;
                  

bits(64) RMR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL1 && IsHighestEL(EL1) then
        return Zeros(32):RMR_EL1;
    else
        UNDEFINED;
                  

bits(64) TTBR0_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return TTBR0_EL3;
                  

bits(64) TRFCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return TRFCR_EL2;
    elsif PSTATE.EL == EL3 then
        return TRFCR_EL2;
                  

bits(64) TRFCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TTRF == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x880];
        else
            return TRFCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return TRFCR_EL2;
        else
            return TRFCR_EL1;
    elsif PSTATE.EL == EL3 then
        return TRFCR_EL1;
                  

bits(64) TTBR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TTBR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x200];
        else
            return TTBR0_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TTBR0_EL2;
        else
            return TTBR0_EL1;
    elsif PSTATE.EL == EL3 then
        return TTBR0_EL1;
                  

bits(64) TTBR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x200];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TTBR0_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return TTBR0_EL1;
        else
            UNDEFINED;
                  

bits(64) S3_op1_Cn_Cm_op2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.TIDCP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            IMPLEMENTATION_DEFINED "";
    else
        IMPLEMENTATION_DEFINED "";
                  

bits(64) RMR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL3 && IsHighestEL(EL3) then
        return Zeros(32):RMR_EL3;
    else
        UNDEFINED;
                  

bits(64) AMCFGR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCFGR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCFGR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCFGR_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCFGR_EL0;
                  

bits(64) LORID_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.LORID_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORID_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORID_EL1;
    elsif PSTATE.EL == EL3 then
        return LORID_EL1;
                  

bits(64) SCTLR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return SCTLR_EL2;
    elsif PSTATE.EL == EL3 then
        return SCTLR_EL2;
                  

bits(64) SCTLR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.SCTLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x110];
        else
            return SCTLR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return SCTLR_EL2;
        else
            return SCTLR_EL1;
    elsif PSTATE.EL == EL3 then
        return SCTLR_EL1;
                  

bits(64) PAN_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        return Zeros(41):PSTATE.PAN:Zeros(22);
    elsif PSTATE.EL == EL2 then
        return Zeros(41):PSTATE.PAN:Zeros(22);
    elsif PSTATE.EL == EL3 then
        return Zeros(41):PSTATE.PAN:Zeros(22);
                  

bits(64) SDER32_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SDER32_EL3;
                  

bits(64) VSTCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x048];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return Zeros(32):VSTCR_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return Zeros(32):VSTCR_EL2;
                  

bits(64) MPAMVPM6_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x970];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM6_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM6_EL2;
                  

bits(64) DBGAUTHSTATUS_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGAUTHSTATUS_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGAUTHSTATUS_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGAUTHSTATUS_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DBGAUTHSTATUS_EL1;
                  

bits(64) CNTVCTSS_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VCTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTHCTL_EL2.EL1TVCT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTVCTSS_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTHCTL_EL2.EL1TVCT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTVCTSS_EL0;
    elsif PSTATE.EL == EL2 then
        return CNTVCTSS_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTVCTSS_EL0;
                  

bits(64) ID_ISAR6_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!IsZero(ID_ISAR6_EL1) || boolean IMPLEMENTATION_DEFINED "ID_ISAR6_EL1 trapped by HCR_EL2.TID3") && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR6_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR6_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR6_EL1;
                  

bits(64) ICV_HPPIR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_HPPIR1_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR1_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR1_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR1_EL1;
                  

bits(64) REVIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.REVIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):REVIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):REVIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):REVIDR_EL1;
                  

bits(64) HFGWTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1C0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return HFGWTR_EL2;
    elsif PSTATE.EL == EL3 then
        return HFGWTR_EL2;
                  

bits(64) SDER32_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):SDER32_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SDER32_EL2;
                  

bits(64) PMXEVCNTR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.[ER,EN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMXEVCNTR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMXEVCNTR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMXEVCNTR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMXEVCNTR_EL0;
                  

bits(64) SCTLR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return SCTLR_EL3;
                  

bits(64) RMR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL1 && EL2Enabled() && IsHighestEL(EL2) && HCR_EL2.NV == '1' then
        AArch64.SystemAccessTrap(EL2, 0x18);
    elsif PSTATE.EL == EL2 && IsHighestEL(EL2) then
        return Zeros(32):RMR_EL2;
    else
        UNDEFINED;
                  

bits(64) CNTV_CVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHVS_CVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return CNTHV_CVAL_EL2;
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x168];
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHVS_CVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return CNTHV_CVAL_EL2;
        else
            return CNTV_CVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTV_CVAL_EL0;
                  

bits(64) CNTV_CVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVVCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                return NVMem[0x168];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return CNTV_CVAL_EL0;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return CNTV_CVAL_EL0;
        else
            UNDEFINED;
                  

bits(64) ERRSELR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERRSELR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERRSELR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERRSELR_EL1;
    elsif PSTATE.EL == EL3 then
        return ERRSELR_EL1;
                  

bits(64) ICC_RPR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_RPR_EL1;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_RPR_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_RPR_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_RPR_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_RPR_EL1;
                  

bits(64) ICC_PMR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_PMR_EL1;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_PMR_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_PMR_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_PMR_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_PMR_EL1;
                  

bits(64) ICC_CTLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_CTLR_EL1;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_CTLR_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_CTLR_EL1;
            else
                return Zeros(32):ICC_CTLR_EL1;
        else
            return Zeros(32):ICC_CTLR_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_CTLR_EL1;
            else
                return Zeros(32):ICC_CTLR_EL1;
        else
            return Zeros(32):ICC_CTLR_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_CTLR_EL1;
            else
                return Zeros(32):ICC_CTLR_EL1;
                  

bits(64) ZCR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            return ZCR_EL3;
                  

bits(64) ERXSTATUS_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXSTATUS_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXSTATUS_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXSTATUS_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXSTATUS_EL1;
                  

bits(64) TTBR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TTBR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x210];
        else
            return TTBR1_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TTBR1_EL2;
        else
            return TTBR1_EL1;
    elsif PSTATE.EL == EL3 then
        return TTBR1_EL1;
                  

bits(64) TTBR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x210];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TTBR1_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return TTBR1_EL1;
        else
            UNDEFINED;
                  

bits(64) CNTVOFF_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x060];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return CNTVOFF_EL2;
    elsif PSTATE.EL == EL3 then
        return CNTVOFF_EL2;
                  

bits(64) MDCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDCR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MDCR_EL2;
                  

bits(64) MPAMVPM7_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x978];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM7_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM7_EL2;
                  

bits(64) FAR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return FAR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return FAR_EL2;
    elsif PSTATE.EL == EL3 then
        return FAR_EL2;
                  

bits(64) FAR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.FAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x220];
        else
            return FAR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return FAR_EL2;
        else
            return FAR_EL1;
    elsif PSTATE.EL == EL3 then
        return FAR_EL1;
                  

bits(64) TFSR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return TFSR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return TFSR_EL2;
    elsif PSTATE.EL == EL3 then
        return TFSR_EL2;
                  

bits(64) TFSR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x190];
        else
            return TFSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return TFSR_EL2;
        else
            return TFSR_EL1;
    elsif PSTATE.EL == EL3 then
        return TFSR_EL1;
                  

bits(64) ICV_HPPIR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_HPPIR0_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR0_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR0_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_HPPIR0_EL1;
                  

bits(64) TFSR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return TFSR_EL3;
                  

bits(64) FAR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return FAR_EL3;
                  

bits(64) HFGITR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1C8];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return HFGITR_EL2;
    elsif PSTATE.EL == EL3 then
        return HFGITR_EL2;
                  

bits(64) ERXADDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXADDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXADDR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXADDR_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXADDR_EL1;
                  

bits(64) MDCR_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MDCR_EL3;
                  

bits(64) PMSCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x828];
        else
            return PMSCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return PMSCR_EL2;
        else
            return PMSCR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSCR_EL1;
                  

bits(64) PMSCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x828];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return PMSCR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return PMSCR_EL1;
        else
            UNDEFINED;
                  

bits(64) ZCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            return ZCR_EL2;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            return ZCR_EL2;
                  

bits(64) ZCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x1E0];
        else
            return ZCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif HCR_EL2.E2H == '1' then
            return ZCR_EL2;
        else
            return ZCR_EL1;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            return ZCR_EL1;
                  

bits(64) ERXCTLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXCTLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXCTLR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXCTLR_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXCTLR_EL1;
                  

bits(64) DBGCLAIMSET_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGCLAIM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGCLAIMSET_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGCLAIMSET_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DBGCLAIMSET_EL1;
                  

bits(64) DBGVCR32_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):DBGVCR32_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DBGVCR32_EL2;
                  

bits(64) TTBR1_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return TTBR1_EL2;
    elsif PSTATE.EL == EL3 then
        return TTBR1_EL2;
                  

bits(64) TTBR1_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TTBR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x210];
        else
            return TTBR1_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return TTBR1_EL2;
        else
            return TTBR1_EL1;
    elsif PSTATE.EL == EL3 then
        return TTBR1_EL1;
                  

bits(64) PMBIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        return PMBIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return PMBIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMBIDR_EL1;
                  

bits(64) FAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.FAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x220];
        else
            return FAR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return FAR_EL2;
        else
            return FAR_EL1;
    elsif PSTATE.EL == EL3 then
        return FAR_EL1;
                  

bits(64) FAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x220];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return FAR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return FAR_EL1;
        else
            UNDEFINED;
                  

bits(64) FAR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return FAR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return FAR_EL2;
    elsif PSTATE.EL == EL3 then
        return FAR_EL2;
                  

bits(64) MPAM0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAM2_EL2.TRAPMPAM0EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return MPAM0_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAM0_EL1;
    elsif PSTATE.EL == EL3 then
        return MPAM0_EL1;
                  

bits(64) CNTP_CVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHPS_CVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return CNTHP_CVAL_EL2;
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x178];
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return CNTHPS_CVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return CNTHP_CVAL_EL2;
        else
            return CNTP_CVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTP_CVAL_EL0;
                  

bits(64) CNTP_CVAL_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVPCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                return NVMem[0x178];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return CNTP_CVAL_EL0;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return CNTP_CVAL_EL0;
        else
            UNDEFINED;
                  

bits(64) TFSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x190];
        else
            return TFSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return TFSR_EL2;
        else
            return TFSR_EL1;
    elsif PSTATE.EL == EL3 then
        return TFSR_EL1;
                  

bits(64) TFSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x190];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return TFSR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return TFSR_EL1;
        else
            UNDEFINED;
                  

bits(64) TFSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return TFSR_EL1;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return TFSR_EL2;
    elsif PSTATE.EL == EL3 then
        return TFSR_EL2;
                  

bits(64) ICH_AP1R_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x4A0+8*UInt(op2[1:0])];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ICH_AP1R_EL2[UInt(op2[1:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICH_AP1R_EL2[UInt(op2[1:0])];
                  

bits(64) APDAKeyLo_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APDAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDAKeyLo_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDAKeyLo_EL1;
    elsif PSTATE.EL == EL3 then
        return APDAKeyLo_EL1;
                  

bits(64) CurrentEL_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            return Zeros(60):'10':Zeros(2);
        else
            return Zeros(60):PSTATE.EL:Zeros(2);
    elsif PSTATE.EL == EL2 then
        return Zeros(60):PSTATE.EL:Zeros(2);
    elsif PSTATE.EL == EL3 then
        return Zeros(60):PSTATE.EL:Zeros(2);
                  

bits(64) TFSRE0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return TFSRE0_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return TFSRE0_EL1;
    elsif PSTATE.EL == EL3 then
        return TFSRE0_EL1;
                  

bits(64) AMCR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && AMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return AMCR_EL0;
    elsif PSTATE.EL == EL3 then
        return AMCR_EL0;
                  

bits(64) PMINTENSET_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMINTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMINTENSET_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMINTENSET_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMINTENSET_EL1;
                  

bits(64) PMSCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSCR_EL2;
    elsif PSTATE.EL == EL3 then
        return PMSCR_EL2;
                  

bits(64) PMSCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x828];
        else
            return PMSCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return PMSCR_EL2;
        else
            return PMSCR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSCR_EL1;
                  

bits(64) ZCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x1E0];
        else
            return ZCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif HCR_EL2.E2H == '1' then
            return ZCR_EL2;
        else
            return ZCR_EL1;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            return ZCR_EL1;
                  

bits(64) ZCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x1E0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
                AArch64.SystemAccessTrap(EL2, 0x19);
            elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
                AArch64.SystemAccessTrap(EL2, 0x19);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
                AArch64.SystemAccessTrap(EL3, 0x19);
            else
                return ZCR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            if CPTR_EL3.EZ == '0' then
                AArch64.SystemAccessTrap(EL3, 0x19);
            else
                return ZCR_EL1;
        else
            UNDEFINED;
                  

bits(64) ID_MMFR2_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_MMFR2_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_MMFR2_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_MMFR2_EL1;
                  

bits(64) CONTEXTIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CONTEXTIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x108];
        else
            return Zeros(32):CONTEXTIDR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CONTEXTIDR_EL2;
        else
            return Zeros(32):CONTEXTIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CONTEXTIDR_EL1;
                  

bits(64) CONTEXTIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x108];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CONTEXTIDR_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):CONTEXTIDR_EL1;
        else
            UNDEFINED;
                  

bits(64) MPAMVPM5_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x968];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM5_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM5_EL2;
                  

bits(64) MIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.MIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) then
            return Zeros(32):VPIDR_EL2;
        else
            return Zeros(32):MIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):MIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MIDR_EL1;
                  

bits(64) LORN_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.LORN_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORN_EL1;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return LORN_EL1;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            return LORN_EL1;
                  

bits(64) VSESR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x508];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return VSESR_EL2;
    elsif PSTATE.EL == EL3 then
        return VSESR_EL2;
                  

bits(64) AFSR1_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):AFSR1_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR1_EL2;
                  

bits(64) AFSR1_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AFSR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x130];
        else
            return Zeros(32):AFSR1_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR1_EL2;
        else
            return Zeros(32):AFSR1_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR1_EL1;
                  

bits(64) MPAM2_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAM2_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAM2_EL2;
                  

bits(64) MPAM2_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAM2_EL2.TRAPMPAM1EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x900];
        else
            return MPAM1_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            return MPAM2_EL2;
        else
            return MPAM1_EL1;
    elsif PSTATE.EL == EL3 then
        return MPAM1_EL1;
                  

bits(64) ICH_EISR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ICH_EISR_EL2;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICH_EISR_EL2;
                  

bits(64) SP_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return SP_EL2;
                  

bits(64) APDAKeyHi_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APDAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDAKeyHi_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APDAKeyHi_EL1;
    elsif PSTATE.EL == EL3 then
        return APDAKeyHi_EL1;
                  

bits(64) DBGBVR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGBVRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGBVR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGBVR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGBVR_EL1[UInt(CRm[3:0])];
                  

bits(64) CNTHVS_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return Zeros(32):CNTHVS_CTL_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return Zeros(32):CNTHVS_CTL_EL2;
                  

bits(64) CNTHVS_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_CTL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_CTL_EL2;
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x170];
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_CTL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_CTL_EL2;
        else
            return Zeros(32):CNTV_CTL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTV_CTL_EL0;
                  

bits(64) ICH_LR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x400+8*UInt(CRm[0]:op2[2:0])];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ICH_LR_EL2[UInt(CRm[0]:op2[2:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICH_LR_EL2[UInt(CRm[0]:op2[2:0])];
                  

bits(64) PMCNTENSET_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCNTENSET_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCNTENSET_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCNTENSET_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMCNTENSET_EL0;
                  

bits(64) HCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x078];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return HCR_EL2;
    elsif PSTATE.EL == EL3 then
        return HCR_EL2;
                  

bits(64) AFSR1_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR1_EL3;
                  

bits(64) ID_ISAR5_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR5_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR5_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR5_EL1;
                  

bits(64) ID_AA64ISAR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64ISAR1_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64ISAR1_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64ISAR1_EL1;
                  

bits(64) MDCCINT_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDCCINT_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDCCINT_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MDCCINT_EL1;
                  

bits(64) DBGBCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.DBGBCRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGBCR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGBCR_EL1[UInt(CRm[3:0])];
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            return DBGBCR_EL1[UInt(CRm[3:0])];
                  

bits(64) PMUSERENR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMUSERENR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMUSERENR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMUSERENR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMUSERENR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMUSERENR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMUSERENR_EL0;
                  

bits(64) CONTEXTIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CONTEXTIDR_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CONTEXTIDR_EL2;
                  

bits(64) CONTEXTIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CONTEXTIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x108];
        else
            return Zeros(32):CONTEXTIDR_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CONTEXTIDR_EL2;
        else
            return Zeros(32):CONTEXTIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CONTEXTIDR_EL1;
                  

bits(64) CNTPS_CVAL_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '0' then
            if SCR_EL3.EEL2 == '1' then
                UNDEFINED;
            elsif SCR_EL3.ST == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                return CNTPS_CVAL_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return CNTPS_CVAL_EL1;
                  

bits(64) FPCR_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CPACR_EL1.FPEN != '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x00);
            else
                AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CPTR_EL2.FPEN != '11' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPCR;
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPCR;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPCR;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPCR;
                  

bits(64) AFSR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AFSR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x130];
        else
            return Zeros(32):AFSR1_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR1_EL2;
        else
            return Zeros(32):AFSR1_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR1_EL1;
                  

bits(64) AFSR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x130];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR1_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR1_EL1;
        else
            UNDEFINED;
                  

bits(64) CNTPCTSS_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PCTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTPCTSS_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CNTHCTL_EL2.EL1PCTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CNTPCTSS_EL0;
    elsif PSTATE.EL == EL2 then
        return CNTPCTSS_EL0;
    elsif PSTATE.EL == EL3 then
        return CNTPCTSS_EL0;
                  

bits(64) CNTHVS_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return Zeros(32):CNTHVS_TVAL_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return Zeros(32):CNTHVS_TVAL_EL2;
                  

bits(64) CNTHVS_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_TVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_TVAL_EL2;
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_TVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_TVAL_EL2;
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTV_TVAL_EL0;
                  

bits(64) SP_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x240];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return SP_EL1;
    elsif PSTATE.EL == EL3 then
        return SP_EL1;
                  

bits(64) SPSR_abt_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):SPSR_abt;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_abt;
                  

bits(64) CLIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID4 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CLIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return CLIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return CLIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return CLIDR_EL1;
                  

bits(64) RNDR_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        return RNDR;
    elsif PSTATE.EL == EL1 then
        return RNDR;
    elsif PSTATE.EL == EL2 then
        return RNDR;
    elsif PSTATE.EL == EL3 then
        return RNDR;
                  

bits(64) DISR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.AMO == '1' then
            return VDISR_EL2;
        else
            return DISR_EL1;
    elsif PSTATE.EL == EL2 then
        return DISR_EL1;
    elsif PSTATE.EL == EL3 then
        return DISR_EL1;
                  

bits(64) ID_MMFR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_MMFR0_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_MMFR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_MMFR0_EL1;
                  

bits(64) SP_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if PSTATE.SP == '0' then
            UNDEFINED;
        else
            return SP_EL0;
    elsif PSTATE.EL == EL2 then
        if PSTATE.SP == '0' then
            UNDEFINED;
        else
            return SP_EL0;
    elsif PSTATE.EL == EL3 then
        if PSTATE.SP == '0' then
            UNDEFINED;
        else
            return SP_EL0;
                  

bits(64) CNTHP_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CNTHP_TVAL_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTHP_TVAL_EL2;
                  

bits(64) CNTHP_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_TVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_TVAL_EL2;
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_TVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_TVAL_EL2;
        else
            return Zeros(32):CNTP_TVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTP_TVAL_EL0;
                  

bits(64) SPSel_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        return Zeros(63):PSTATE.SP;
    elsif PSTATE.EL == EL2 then
        return Zeros(63):PSTATE.SP;
    elsif PSTATE.EL == EL3 then
        return Zeros(63):PSTATE.SP;
                  

bits(64) AFSR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AFSR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x128];
        else
            return Zeros(32):AFSR0_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR0_EL2;
        else
            return Zeros(32):AFSR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR0_EL1;
                  

bits(64) AFSR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            return NVMem[0x128];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR0_EL1;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR0_EL1;
        else
            UNDEFINED;
                  

bits(64) RNDRRS_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        return RNDRRS;
    elsif PSTATE.EL == EL1 then
        return RNDRRS;
    elsif PSTATE.EL == EL2 then
        return RNDRRS;
    elsif PSTATE.EL == EL3 then
        return RNDRRS;
                  

bits(64) ICH_MISR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ICH_MISR_EL2;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICH_MISR_EL2;
                  

bits(64) PMSIDR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMSIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSIDR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMSIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMSIDR_EL1;
                  

bits(64) PMCCNTR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.[CR,EN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCCNTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCCNTR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMCCNTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCCNTR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMCCNTR_EL0;
    elsif PSTATE.EL == EL3 then
        return PMCCNTR_EL0;
                  

bits(64) VPIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x088];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):VPIDR_EL2;
    elsif PSTATE.EL == EL3 then
        if !HaveEL(EL2) then
            return Zeros(32):MIDR_EL1;
        else
            return Zeros(32):VPIDR_EL2;
                  

bits(64) VPIDR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.MIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) then
            return Zeros(32):VPIDR_EL2;
        else
            return Zeros(32):MIDR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):MIDR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MIDR_EL1;
                  

bits(64) VNCR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x0B0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return VNCR_EL2;
    elsif PSTATE.EL == EL3 then
        return VNCR_EL2;
                  

bits(64) CSSELR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID4 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.CSSELR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CSSELR_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CSSELR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CSSELR_EL1;
                  

bits(64) ERXPFGCTL_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FIEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXPFGCTL_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXPFGCTL_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXPFGCTL_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXPFGCTL_EL1;
                  

bits(64) MDSCR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.MDSCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x158];
        else
            return Zeros(32):MDSCR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):MDSCR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):MDSCR_EL1;
                  

bits(64) HDFGWTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1D8];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return HDFGWTR_EL2;
    elsif PSTATE.EL == EL3 then
        return HDFGWTR_EL2;
                  

bits(64) ID_MMFR1_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_MMFR1_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_MMFR1_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_MMFR1_EL1;
                  

bits(64) SPSR_fiq_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):SPSR_fiq;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):SPSR_fiq;
                  

bits(64) HAFGRTR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x1E8];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return HAFGRTR_EL2;
    elsif PSTATE.EL == EL3 then
        return HAFGRTR_EL2;
                  

bits(64) VTTBR_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x020];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return VTTBR_EL2;
    elsif PSTATE.EL == EL3 then
        return VTTBR_EL2;
                  

bits(64) OSDLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" && HDFGRTR_EL2.OSDLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDOSA] != '00' && (boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" || boolean IMPLEMENTATION_DEFINED "Trapped by MDCR_EL2.TDOSA") then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' && (boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" || boolean IMPLEMENTATION_DEFINED "Trapped by MDCR_EL3.TDOSA") then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSDLR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' && (boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" || boolean IMPLEMENTATION_DEFINED "Trapped by MDCR_EL3.TDOSA") then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):OSDLR_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):OSDLR_EL1;
                  

bits(64) PMOVSCLR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMOVSCLR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMOVSCLR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMOVSCLR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMOVSCLR_EL0;
                  

bits(64) CNTHPS_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            return Zeros(32):CNTHPS_CTL_EL2;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            return Zeros(32):CNTHPS_CTL_EL2;
                  

bits(64) CNTHPS_CTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_CTL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_CTL_EL2;
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x180];
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHPS_CTL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHP_CTL_EL2;
        else
            return Zeros(32):CNTP_CTL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTP_CTL_EL0;
                  

bits(64) PMCR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMCR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCR_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMCR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCR_EL0;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):PMCR_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):PMCR_EL0;
                  

bits(64) AFSR0_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):AFSR0_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR0_EL2;
                  

bits(64) AFSR0_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TRVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.AFSR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            return NVMem[0x128];
        else
            return Zeros(32):AFSR0_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):AFSR0_EL2;
        else
            return Zeros(32):AFSR0_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR0_EL1;
                  

bits(64) UAO_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        return Zeros(40):PSTATE.UAO:Zeros(23);
    elsif PSTATE.EL == EL2 then
        return Zeros(40):PSTATE.UAO:Zeros(23);
    elsif PSTATE.EL == EL3 then
        return Zeros(40):PSTATE.UAO:Zeros(23);
                  

bits(64) ERXPFGF_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FIEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.ERXPFGF_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXPFGF_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ERXPFGF_EL1;
    elsif PSTATE.EL == EL3 then
        return ERXPFGF_EL1;
                  

bits(64) MPAMVPM4_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x960];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return MPAMVPM4_EL2;
    elsif PSTATE.EL == EL3 then
        return MPAMVPM4_EL2;
                  

bits(64) ICV_CTLR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_CTLR_EL1;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_CTLR_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_CTLR_EL1;
            else
                return Zeros(32):ICC_CTLR_EL1;
        else
            return Zeros(32):ICC_CTLR_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_CTLR_EL1;
            else
                return Zeros(32):ICC_CTLR_EL1;
        else
            return Zeros(32):ICC_CTLR_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                return Zeros(32):ICC_CTLR_EL1;
            else
                return Zeros(32):ICC_CTLR_EL1;
                  

bits(64) FPEXC32_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPEXC32_EL2;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            return Zeros(32):FPEXC32_EL2;
                  

bits(64) TPIDRRO_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TPIDRRO_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return TPIDRRO_EL0;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.TPIDRRO_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return TPIDRRO_EL0;
    elsif PSTATE.EL == EL2 then
        return TPIDRRO_EL0;
    elsif PSTATE.EL == EL3 then
        return TPIDRRO_EL0;
                  

bits(64) CNTHV_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CNTHV_TVAL_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTHV_TVAL_EL2;
                  

bits(64) CNTHV_TVAL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_TVAL_EL2;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_TVAL_EL2;
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            return Zeros(32):CNTHVS_TVAL_EL2;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            return Zeros(32):CNTHV_TVAL_EL2;
        else
            return Zeros(32):CNTV_TVAL_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTV_TVAL_EL0;
                  

bits(64) CNTHCTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CNTHCTL_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTHCTL_EL2;
                  

bits(64) CNTHCTL_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        return Zeros(32):CNTKCTL_EL1;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            return Zeros(32):CNTHCTL_EL2;
        else
            return Zeros(32):CNTKCTL_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTKCTL_EL1;
                  

bits(64) APGAKeyHi_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APGAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APGAKeyHi_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APGAKeyHi_EL1;
    elsif PSTATE.EL == EL3 then
        return APGAKeyHi_EL1;
                  

bits(64) PMEVCNTR_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.[ER,EN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])];
    elsif PSTATE.EL == EL3 then
        return PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])];
                  

bits(64) DACR32_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):DACR32_EL2;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):DACR32_EL2;
                  

bits(64) ICV_PMR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_PMR_EL1;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_PMR_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_PMR_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_PMR_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_PMR_EL1;
                  

bits(64) ICV_RPR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            return Zeros(32):ICV_RPR_EL1;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            return Zeros(32):ICV_RPR_EL1;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_RPR_EL1;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_RPR_EL1;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return Zeros(32):ICC_RPR_EL1;
                  

bits(64) PMBSR_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGRTR_EL2.PMBSR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.E2PB == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            return NVMem[0x820];
        else
            return PMBSR_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return PMBSR_EL1;
    elsif PSTATE.EL == EL3 then
        return PMBSR_EL1;
                  

bits(64) CNTFRQ_EL0_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.[EL0PCTEN,EL0VCTEN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.[EL0PCTEN,EL0VCTEN] == '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):CNTFRQ_EL0;
    elsif PSTATE.EL == EL1 then
        return Zeros(32):CNTFRQ_EL0;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):CNTFRQ_EL0;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):CNTFRQ_EL0;
                  

bits(64) APIAKeyLo_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGRTR_EL2.APIAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIAKeyLo_EL1;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return APIAKeyLo_EL1;
    elsif PSTATE.EL == EL3 then
        return APIAKeyLo_EL1;
                  

bits(64) ID_AA64ISAR0_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ID_AA64ISAR0_EL1;
    elsif PSTATE.EL == EL2 then
        return ID_AA64ISAR0_EL1;
    elsif PSTATE.EL == EL3 then
        return ID_AA64ISAR0_EL1;
                  

bits(64) ICH_AP0R_EL2_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            return NVMem[0x480+8*UInt(op2[1:0])];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return ICH_AP0R_EL2[UInt(op2[1:0])];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            return ICH_AP0R_EL2[UInt(op2[1:0])];
                  

bits(64) MPAM3_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return MPAM3_EL3;
                  

bits(64) ID_ISAR4_EL1_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        if boolean IMPLEMENTATION_DEFINED "ARMv8.4-IDST" then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID3 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            return Zeros(32):ID_ISAR4_EL1;
    elsif PSTATE.EL == EL2 then
        return Zeros(32):ID_ISAR4_EL1;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):ID_ISAR4_EL1;
                  

bits(64) AFSR0_EL3_reg_read(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        return Zeros(32):AFSR0_EL3;
                  

CNTPS_CTL_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '0' then
            if SCR_EL3.EEL2 == '1' then
                UNDEFINED;
            elsif SCR_EL3.ST == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                CNTPS_CTL_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        CNTPS_CTL_EL1 = val[31:0];
                  

ERXMISC2_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC2_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC2_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXMISC2_EL1 = val;
                  

VBAR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VBAR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        VBAR_EL2 = val;
                  

VBAR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.VBAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x250] = val;
        else
            VBAR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            VBAR_EL2 = val;
        else
            VBAR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        VBAR_EL1 = val;
                  

ICC_EOIR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_EOIR1_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR1_EL1 = val[31:0];
                  

ICC_DIR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TDIR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_DIR_EL1 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_DIR_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_DIR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_DIR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_DIR_EL1 = val[31:0];
                  

CNTHPS_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            CNTHPS_TVAL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            CNTHPS_TVAL_EL2 = val[31:0];
                  

CNTHPS_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_TVAL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_TVAL_EL2 = val[31:0];
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_TVAL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_TVAL_EL2 = val[31:0];
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTP_TVAL_EL0 = val[31:0];
                  

ICC_BPR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_BPR1_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_BPR1_EL1 = val[31:0];
            else
                ICC_BPR1_EL1 = val[31:0];
        else
            ICC_BPR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_BPR1_EL1 = val[31:0];
            else
                ICC_BPR1_EL1 = val[31:0];
        else
            ICC_BPR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_BPR1_EL1 = val[31:0];
            else
                ICC_BPR1_EL1 = val[31:0];
                  

ACTLR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        ACTLR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        ACTLR_EL2 = val;
                  

AMCNTENCLR1_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMCNTENCLR1_EL0 = val;
    else
        UNDEFINED;
                  

DBGWVR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGWVRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGWVR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGWVR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGWVR_EL1[UInt(CRm[3:0])] = val;
                  

CPACR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.CPACR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x100] = val;
        else
            CPACR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            CPTR_EL2 = val[31:0];
        else
            CPACR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CPACR_EL1 = val[31:0];
                  

CPACR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x100] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                CPACR_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CPACR_EL1 = val[31:0];
        else
            UNDEFINED;
                  

CNTKCTL_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        CNTKCTL_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTHCTL_EL2 = val[31:0];
        else
            CNTKCTL_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTKCTL_EL1 = val[31:0];
                  

CNTKCTL_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTKCTL_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTKCTL_EL1 = val[31:0];
        else
            UNDEFINED;
                  

APDBKeyHi_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APDBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDBKeyHi_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDBKeyHi_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APDBKeyHi_EL1 = val;
                  

ICC_SGI0R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SGI0R_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SGI0R_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SGI0R_EL1 = val;
                  

ICC_SGI1R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SGI1R_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SGI1R_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SGI1R_EL1 = val;
                  

ACTLR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        ACTLR_EL3 = val;
                  

ICC_SRE_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && ICC_SRE_EL2.Enable == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && ICC_SRE_EL3.Enable == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_SRE_EL1 = val[31:0];
            else
                ICC_SRE_EL1 = val[31:0];
        else
            ICC_SRE_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && ICC_SRE_EL3.Enable == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_SRE_EL1 = val[31:0];
            else
                ICC_SRE_EL1 = val[31:0];
        else
            ICC_SRE_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            ICC_SRE_EL1 = val[31:0];
        else
            ICC_SRE_EL1 = val[31:0];
                  

MPAMVPM1_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x948] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM1_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM1_EL2 = val;
                  

PMOVSSET_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMOVSSET_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMOVSSET_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMOVSSET_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMOVSSET_EL0 = val[31:0];
                  

DBGWCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGWCRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGWCR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGWCR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGWCR_EL1[UInt(CRm[3:0])] = val;
                  

SPSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x160] = val;
        else
            SPSR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            SPSR_EL2 = val[31:0];
        else
            SPSR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_EL1 = val[31:0];
                  

SPSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x160] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            SPSR_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            SPSR_EL1 = val[31:0];
        else
            UNDEFINED;
                  

SPSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            SPSR_EL1 = val[31:0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SPSR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_EL2 = val[31:0];
                  

ELR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x230] = val;
        else
            ELR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            ELR_EL2 = val;
        else
            ELR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ELR_EL1 = val;
                  

ELR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x230] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            ELR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            ELR_EL1 = val;
        else
            UNDEFINED;
                  

ELR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            ELR_EL1 = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        ELR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        ELR_EL2 = val;
                  

VBAR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        VBAR_EL3 = val;
                  

HDFGRTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1D0] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            HDFGRTR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HDFGRTR_EL2 = val;
                  

VBAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.VBAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x250] = val;
        else
            VBAR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            VBAR_EL2 = val;
        else
            VBAR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        VBAR_EL1 = val;
                  

VBAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x250] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            VBAR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            VBAR_EL1 = val;
        else
            UNDEFINED;
                  

PMBPTR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMBPTR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.E2PB == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x810] = val;
        else
            PMBPTR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMBPTR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMBPTR_EL1 = val;
                  

AMCNTENSET0_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMCNTENSET0_EL0 = val;
    else
        UNDEFINED;
                  

ELR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        ELR_EL3 = val;
                  

SPSR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        SPSR_EL3 = val[31:0];
                  

SCR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        SCR_EL3 = val;
                  

ICC_SRE_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        ICC_SRE_EL3 = val[31:0];
                  

ACTLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TACR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x118] = val;
        else
            ACTLR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        ACTLR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ACTLR_EL1 = val;
                  

CNTV_TVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_TVAL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_TVAL_EL2 = val[31:0];
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_TVAL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_TVAL_EL2 = val[31:0];
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTV_TVAL_EL0 = val[31:0];
                  

CNTV_TVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTV_TVAL_EL0 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTV_TVAL_EL0 = val[31:0];
        else
            UNDEFINED;
                  

ICV_EOIR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_EOIR1_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR1_EL1 = val[31:0];
                  

ICC_SRE_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && ICC_SRE_EL3.Enable == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_SRE_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if !EL2Enabled() then
            UNDEFINED;
        else
            ICC_SRE_EL2 = val[31:0];
                  

PMSFCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSFCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSFCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSFCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSFCR_EL1 = val;
                  

VDISR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x500] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VDISR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        VDISR_EL2 = val;
                  

VDISR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.AMO == '1' then
            VDISR_EL2 = val;
        else
            DISR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        DISR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        DISR_EL1 = val;
                  

HPFAR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        HPFAR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HPFAR_EL2 = val;
                  

SPSR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            SPSR_EL1 = val[31:0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SPSR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_EL2 = val[31:0];
                  

SPSR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x160] = val;
        else
            SPSR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            SPSR_EL2 = val[31:0];
        else
            SPSR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_EL1 = val[31:0];
                  

PMSELR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.[ER,EN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSELR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSELR_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSELR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSELR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSELR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMSELR_EL0 = val[31:0];
                  

ELR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            ELR_EL1 = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        ELR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        ELR_EL2 = val;
                  

ELR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x230] = val;
        else
            ELR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            ELR_EL2 = val;
        else
            ELR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ELR_EL1 = val;
                  

LORC_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.LORC_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LORC_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LORC_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            LORC_EL1 = val;
                  

LOREA_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.LOREA_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LOREA_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LOREA_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            LOREA_EL1 = val;
                  

SCXTNUM_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.SCXTNUM_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x188] = val;
        else
            SCXTNUM_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            SCXTNUM_EL2 = val;
        else
            SCXTNUM_EL1 = val;
    elsif PSTATE.EL == EL3 then
        SCXTNUM_EL1 = val;
                  

SCXTNUM_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x188] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                SCXTNUM_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            SCXTNUM_EL1 = val;
        else
            UNDEFINED;
                  

TPIDR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x090] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        TPIDR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TPIDR_EL2 = val;
                  

AMCNTENSET1_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMCNTENSET1_EL0 = val;
    else
        UNDEFINED;
                  

ICC_ASGI1R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_ASGI1R_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_ASGI1R_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_ASGI1R_EL1 = val;
                  

ICV_AP1R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_AP1R_EL1[UInt(op2[1:0])] = val;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
            else
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
        else
            ICC_AP1R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
            else
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
        else
            ICC_AP1R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
            else
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
                  

AMAIR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        AMAIR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        AMAIR_EL2 = val;
                  

AMAIR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.AMAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x148] = val;
        else
            AMAIR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AMAIR_EL2 = val;
        else
            AMAIR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        AMAIR_EL1 = val;
                  

CNTV_CTL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CTL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_CTL_EL2 = val[31:0];
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x170] = val;
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CTL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_CTL_EL2 = val[31:0];
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTV_CTL_EL0 = val[31:0];
                  

CNTV_CTL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVVCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                NVMem[0x170] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTV_CTL_EL0 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTV_CTL_EL0 = val[31:0];
        else
            UNDEFINED;
                  

ESR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            ESR_EL1 = val[31:0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        ESR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        ESR_EL2 = val[31:0];
                  

ESR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ESR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x138] = val;
        else
            ESR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            ESR_EL2 = val[31:0];
        else
            ESR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        ESR_EL1 = val[31:0];
                  

PMEVTYPER_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL3 then
        PMEVTYPER_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
                  

ESR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        ESR_EL3 = val[31:0];
                  

AMAIR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        AMAIR_EL3 = val;
                  

RGSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            RGSR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            RGSR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        RGSR_EL1 = val[31:0];
                  

ICV_EOIR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_EOIR0_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR0_EL1 = val[31:0];
                  

SCXTNUM_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && SCTLR_EL1.TSCXT == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] != '11' && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.SCXTNUM_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCTLR_EL2.TSCXT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            SCXTNUM_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.SCXTNUM_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            SCXTNUM_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            SCXTNUM_EL0 = val;
    elsif PSTATE.EL == EL3 then
        SCXTNUM_EL0 = val;
                  

TPIDR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        TPIDR_EL3 = val;
                  

PMSLATFR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSLATFR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x848] = val;
        else
            PMSLATFR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSLATFR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSLATFR_EL1 = val;
                  

PMCNTENCLR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCNTENCLR_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCNTENCLR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCNTENCLR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMCNTENCLR_EL0 = val[31:0];
                  

TPIDR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TPIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            TPIDR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        TPIDR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TPIDR_EL1 = val;
                  

ERXMISC3_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC3_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC3_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXMISC3_EL1 = val;
                  

SCXTNUM_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            SCXTNUM_EL2 = val;
    elsif PSTATE.EL == EL3 then
        SCXTNUM_EL2 = val;
                  

SCXTNUM_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.SCXTNUM_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x188] = val;
        else
            SCXTNUM_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.EnSCXT == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            SCXTNUM_EL2 = val;
        else
            SCXTNUM_EL1 = val;
    elsif PSTATE.EL == EL3 then
        SCXTNUM_EL1 = val;
                  

OSDTRTX_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSDTRTX_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSDTRTX_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        OSDTRTX_EL1 = val[31:0];
                  

PMSEVFR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSEVFR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x830] = val;
        else
            PMSEVFR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSEVFR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSEVFR_EL1 = val;
                  

ICC_EOIR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_EOIR0_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_EOIR0_EL1 = val[31:0];
                  

CNTHV_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHV_CTL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTHV_CTL_EL2 = val[31:0];
                  

CNTHV_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CTL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_CTL_EL2 = val[31:0];
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x170] = val;
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CTL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_CTL_EL2 = val[31:0];
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTV_CTL_EL0 = val[31:0];
                  

VMPIDR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x050] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VMPIDR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        if !HaveEL(EL2) then
            //no operation
        else
            VMPIDR_EL2 = val;
                  

DBGPRCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGPRCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDOSA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGPRCR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGPRCR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        DBGPRCR_EL1 = val[31:0];
                  

AMAIR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.AMAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x148] = val;
        else
            AMAIR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AMAIR_EL2 = val;
        else
            AMAIR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        AMAIR_EL1 = val;
                  

AMAIR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x148] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AMAIR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            AMAIR_EL1 = val;
        else
            UNDEFINED;
                  

ICC_BPR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_BPR0_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_BPR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_BPR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_BPR0_EL1 = val[31:0];
                  

ESR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ESR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x138] = val;
        else
            ESR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            ESR_EL2 = val[31:0];
        else
            ESR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        ESR_EL1 = val[31:0];
                  

ESR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x138] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            ESR_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            ESR_EL1 = val[31:0];
        else
            UNDEFINED;
                  

ESR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            ESR_EL1 = val[31:0];
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        ESR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        ESR_EL2 = val[31:0];
                  

AMCNTENCLR0_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMCNTENCLR0_EL0 = val;
    else
        UNDEFINED;
                  

APIBKeyLo_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APIBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIBKeyLo_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIBKeyLo_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APIBKeyLo_EL1 = val;
                  

SSBS_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        PSTATE.SSBS = val[12];
    elsif PSTATE.EL == EL1 then
        PSTATE.SSBS = val[12];
    elsif PSTATE.EL == EL2 then
        PSTATE.SSBS = val[12];
    elsif PSTATE.EL == EL3 then
        PSTATE.SSBS = val[12];
                  

MPAMVPM0_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x940] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM0_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM0_EL2 = val;
                  

MPAMHCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x930] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMHCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMHCR_EL2 = val;
                  

CNTP_TVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_TVAL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_TVAL_EL2 = val[31:0];
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_TVAL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_TVAL_EL2 = val[31:0];
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTP_TVAL_EL0 = val[31:0];
                  

CNTP_TVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTP_TVAL_EL0 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTP_TVAL_EL0 = val[31:0];
        else
            UNDEFINED;
                  

ICC_AP1R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_AP1R_EL1[UInt(op2[1:0])] = val;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
            else
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
        else
            ICC_AP1R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
            else
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
        else
            ICC_AP1R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
            else
                ICC_AP1R_EL1[UInt(op2[1:0])] = val;
                  

TPIDR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TPIDR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            TPIDR_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TPIDR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            TPIDR_EL0 = val;
    elsif PSTATE.EL == EL2 then
        TPIDR_EL0 = val;
    elsif PSTATE.EL == EL3 then
        TPIDR_EL0 = val;
                  

SCXTNUM_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        SCXTNUM_EL3 = val;
                  

PMBLIMITR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMBLIMITR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.E2PB == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x800] = val;
        else
            PMBLIMITR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMBLIMITR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMBLIMITR_EL1 = val;
                  

DSPSR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if !Halted() then
        UNDEFINED;
    else
        DSPSR_EL0 = val[31:0];
                  

MAIR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.MAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x140] = val;
        else
            MAIR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            MAIR_EL2 = val;
        else
            MAIR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        MAIR_EL1 = val;
                  

MAIR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x140] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            MAIR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            MAIR_EL1 = val;
        else
            UNDEFINED;
                  

GCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            GCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            GCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        GCR_EL1 = val;
                  

DLR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if !Halted() then
        UNDEFINED;
    else
        DLR_EL0 = val;
                  

ICV_IGRPEN0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_IGRPEN0_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN0_EL1 = val[31:0];
                  

DBGDTRTX_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if Halted() then
        DBGDTRTX_EL0 = val[31:0];
    elsif PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && MDSCR_EL1.TDCC == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (HCR_EL2.TGE == '1' || MDCR_EL2.[TDE,TDA] != '00') then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGDTRTX_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGDTRTX_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGDTRTX_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        DBGDTRTX_EL0 = val[31:0];
                  

ICC_IGRPEN0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_IGRPEN0_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN0_EL1 = val[31:0];
                  

APIBKeyHi_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APIBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIBKeyHi_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIBKeyHi_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APIBKeyHi_EL1 = val;
                  

AMUSERENR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TAM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            AMUSERENR_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TAM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            AMUSERENR_EL0 = val;
    elsif PSTATE.EL == EL3 then
        AMUSERENR_EL0 = val;
                  

OSDTRRX_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSDTRRX_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSDTRRX_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        OSDTRRX_EL1 = val[31:0];
                  

MAIR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        MAIR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MAIR_EL2 = val;
                  

MAIR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.MAIR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x140] = val;
        else
            MAIR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            MAIR_EL2 = val;
        else
            MAIR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        MAIR_EL1 = val;
                  

DBGCLAIMCLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGCLAIM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGCLAIMCLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGCLAIMCLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        DBGCLAIMCLR_EL1 = val[31:0];
                  

PMXEVTYPER_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMXEVTYPER_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVTYPERn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMXEVTYPER_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMXEVTYPER_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMXEVTYPER_EL0 = val[31:0];
                  

DIT_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        PSTATE.DIT = val[24];
    elsif PSTATE.EL == EL1 then
        PSTATE.DIT = val[24];
    elsif PSTATE.EL == EL2 then
        PSTATE.DIT = val[24];
    elsif PSTATE.EL == EL3 then
        PSTATE.DIT = val[24];
                  

CNTHP_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHP_CVAL_EL2 = val;
    elsif PSTATE.EL == EL3 then
        CNTHP_CVAL_EL2 = val;
                  

CNTHP_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CVAL_EL2 = val;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_CVAL_EL2 = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x178] = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CVAL_EL2 = val;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_CVAL_EL2 = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL3 then
        CNTP_CVAL_EL0 = val;
                  

ERXMISC1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC1_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC1_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXMISC1_EL1 = val;
                  

PMINTENCLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMINTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMINTENCLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMINTENCLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMINTENCLR_EL1 = val[31:0];
                  

ICV_BPR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_BPR0_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_BPR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_BPR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_BPR0_EL1 = val[31:0];
                  

CNTPS_TVAL_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '0' then
            if SCR_EL3.EEL2 == '1' then
                UNDEFINED;
            elsif SCR_EL3.ST == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                CNTPS_TVAL_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        CNTPS_TVAL_EL1 = val[31:0];
                  

AMEVTYPER1_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMEVTYPER1_EL0[UInt(CRm[0]:op2[2:0])] = val;
    else
        UNDEFINED;
                  

MPAMVPM2_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x950] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM2_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM2_EL2 = val;
                  

NZCV_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        PSTATE.[N,Z,C,V] = val[31:28];
    elsif PSTATE.EL == EL1 then
        PSTATE.[N,Z,C,V] = val[31:28];
    elsif PSTATE.EL == EL2 then
        PSTATE.[N,Z,C,V] = val[31:28];
    elsif PSTATE.EL == EL3 then
        PSTATE.[N,Z,C,V] = val[31:28];
                  

MAIR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        MAIR_EL3 = val;
                  

SPSR_und_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SPSR_und = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_und = val[31:0];
                  

CNTHVS_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            CNTHVS_CVAL_EL2 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            CNTHVS_CVAL_EL2 = val;
                  

CNTHVS_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CVAL_EL2 = val;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_CVAL_EL2 = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x168] = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CVAL_EL2 = val;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_CVAL_EL2 = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL3 then
        CNTV_CVAL_EL0 = val;
                  

ICV_AP0R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_AP0R_EL1[UInt(op2[1:0])] = val;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_AP0R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_AP0R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_AP0R_EL1[UInt(op2[1:0])] = val;
                  

AMEVCNTVOFF0_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0xA00+8*UInt(CRm[0]:op2[2:0])] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        AMEVCNTVOFF0_EL2[UInt(CRm[0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL3 then
        AMEVCNTVOFF0_EL2[UInt(CRm[0]:op2[2:0])] = val;
                  

AMEVCNTVOFF1_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0xA80+8*UInt(CRm[0]:op2[2:0])] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        AMEVCNTVOFF1_EL2[UInt(CRm[0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL3 then
        AMEVCNTVOFF1_EL2[UInt(CRm[0]:op2[2:0])] = val;
                  

TCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x120] = val;
        else
            TCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TCR_EL2 = val;
        else
            TCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TCR_EL1 = val;
                  

TCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x120] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TCR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            TCR_EL1 = val;
        else
            UNDEFINED;
                  

ERXMISC0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXMISCn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC0_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXMISC0_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXMISC0_EL1 = val;
                  

ICV_DIR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TDIR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_DIR_EL1 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_DIR_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_DIR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_DIR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_DIR_EL1 = val[31:0];
                  

ICC_IGRPEN1_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_IGRPEN1_EL3 = val[31:0];
                  

MPAMVPMV_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x938] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPMV_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPMV_EL2 = val;
                  

TCO_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        PSTATE.TCO = val[25];
    elsif PSTATE.EL == EL1 then
        PSTATE.TCO = val[25];
    elsif PSTATE.EL == EL2 then
        PSTATE.TCO = val[25];
    elsif PSTATE.EL == EL3 then
        PSTATE.TCO = val[25];
                  

ICV_BPR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_BPR1_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_BPR1_EL1 = val[31:0];
            else
                ICC_BPR1_EL1 = val[31:0];
        else
            ICC_BPR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_BPR1_EL1 = val[31:0];
            else
                ICC_BPR1_EL1 = val[31:0];
        else
            ICC_BPR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_BPR1_EL1 = val[31:0];
            else
                ICC_BPR1_EL1 = val[31:0];
                  

CNTP_CTL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CTL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_CTL_EL2 = val[31:0];
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x180] = val;
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CTL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_CTL_EL2 = val[31:0];
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTP_CTL_EL0 = val[31:0];
                  

CNTP_CTL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVPCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                NVMem[0x180] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTP_CTL_EL0 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTP_CTL_EL0 = val[31:0];
        else
            UNDEFINED;
                  

DAIF_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && ((EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') || SCTLR_EL1.UMA == '0') then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        else
            PSTATE.[D,A,I,F] = val[9:6];
    elsif PSTATE.EL == EL1 then
        PSTATE.[D,A,I,F] = val[9:6];
    elsif PSTATE.EL == EL2 then
        PSTATE.[D,A,I,F] = val[9:6];
    elsif PSTATE.EL == EL3 then
        PSTATE.[D,A,I,F] = val[9:6];
                  

PMCCFILTR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCCFILTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCCFILTR_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCCFILTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCCFILTR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCCFILTR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMCCFILTR_EL0 = val[31:0];
                  

MPAMVPM3_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x958] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM3_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM3_EL2 = val;
                  

OSLAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.OSLAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDOSA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSLAR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSLAR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        OSLAR_EL1 = val[31:0];
                  

HACR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        HACR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        HACR_EL2 = val[31:0];
                  

PAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.PAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            PAR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        PAR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PAR_EL1 = val;
                  

TCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        TCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TCR_EL2 = val;
                  

TCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x120] = val;
        else
            TCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TCR_EL2 = val;
        else
            TCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TCR_EL1 = val;
                  

APDBKeyLo_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APDBKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDBKeyLo_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDBKeyLo_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APDBKeyLo_EL1 = val;
                  

ICV_IGRPEN1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_IGRPEN1_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_IGRPEN1_EL1 = val[31:0];
            else
                ICC_IGRPEN1_EL1 = val[31:0];
        else
            ICC_IGRPEN1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_IGRPEN1_EL1 = val[31:0];
            else
                ICC_IGRPEN1_EL1 = val[31:0];
        else
            ICC_IGRPEN1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_IGRPEN1_EL1 = val[31:0];
            else
                ICC_IGRPEN1_EL1 = val[31:0];
                  

VTCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x040] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VTCR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        VTCR_EL2 = val[31:0];
                  

OSECCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.OSECCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSECCR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSECCR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        OSECCR_EL1 = val[31:0];
                  

CNTHP_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHP_CTL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTHP_CTL_EL2 = val[31:0];
                  

CNTHP_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CTL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_CTL_EL2 = val[31:0];
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x180] = val;
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CTL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_CTL_EL2 = val[31:0];
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTP_CTL_EL0 = val[31:0];
                  

ICC_AP0R_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_AP0R_EL1[UInt(op2[1:0])] = val;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_AP0R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_AP0R_EL1[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_AP0R_EL1[UInt(op2[1:0])] = val;
                  

DBGDTR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if Halted() then
        DBGDTR_EL0 = val;
    elsif PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && MDSCR_EL1.TDCC == '1' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (HCR_EL2.TGE == '1' || MDCR_EL2.[TDE,TDA] != '00') then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGDTR_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGDTR_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGDTR_EL0 = val;
    elsif PSTATE.EL == EL3 then
        DBGDTR_EL0 = val;
                  

ICC_IGRPEN1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ICC_IGRPENn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TALL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_IGRPEN1_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_IGRPEN1_EL1 = val[31:0];
            else
                ICC_IGRPEN1_EL1 = val[31:0];
        else
            ICC_IGRPEN1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.IRQ == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_IGRPEN1_EL1 = val[31:0];
            else
                ICC_IGRPEN1_EL1 = val[31:0];
        else
            ICC_IGRPEN1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_IGRPEN1_EL1 = val[31:0];
            else
                ICC_IGRPEN1_EL1 = val[31:0];
                  

CNTPOFF_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1A8] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ECVEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            CNTPOFF_EL2 = val;
    elsif PSTATE.EL == EL3 then
        CNTPOFF_EL2 = val;
                  

HFGRTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1B8] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            HFGRTR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HFGRTR_EL2 = val;
                  

TCR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        TCR_EL3 = val[31:0];
                  

FPSR_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CPACR_EL1.FPEN != '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x00);
            else
                AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CPTR_EL2.FPEN != '11' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPSR = val[31:0];
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPSR = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPSR = val[31:0];
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPSR = val[31:0];
                  

ERXPFGCDN_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FIEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXPFGCDN_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXPFGCDN_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXPFGCDN_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXPFGCDN_EL1 = val;
                  

PMSIRR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSIRR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x840] = val;
        else
            PMSIRR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSIRR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSIRR_EL1 = val;
                  

CNTHV_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHV_CVAL_EL2 = val;
    elsif PSTATE.EL == EL3 then
        CNTHV_CVAL_EL2 = val;
                  

CNTHV_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CVAL_EL2 = val;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_CVAL_EL2 = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x168] = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CVAL_EL2 = val;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_CVAL_EL2 = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL3 then
        CNTV_CVAL_EL0 = val;
                  

TTBR0_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        TTBR0_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TTBR0_EL2 = val;
                  

TTBR0_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TTBR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x200] = val;
        else
            TTBR0_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TTBR0_EL2 = val;
        else
            TTBR0_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TTBR0_EL1 = val;
                  

LORSA_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.LORSA_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LORSA_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LORSA_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            LORSA_EL1 = val;
                  

ICC_CTLR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_CTLR_EL3 = val[31:0];
                  

TRFCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.TRFCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TTRF == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x880] = val;
        else
            TRFCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            TRFCR_EL2 = val;
        else
            TRFCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TRFCR_EL1 = val;
                  

TRFCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x880] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                TRFCR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            TRFCR_EL1 = val;
        else
            UNDEFINED;
                  

PMSICR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSICR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x838] = val;
        else
            PMSICR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSICR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSICR_EL1 = val;
                  

SCTLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.SCTLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x110] = val;
        else
            SCTLR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            SCTLR_EL2 = val;
        else
            SCTLR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        SCTLR_EL1 = val;
                  

SCTLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x110] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            SCTLR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            SCTLR_EL1 = val;
        else
            UNDEFINED;
                  

MPAM1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAM2_EL2.TRAPMPAM1EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x900] = val;
        else
            MPAM1_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            MPAM2_EL2 = val;
        else
            MPAM1_EL1 = val;
    elsif PSTATE.EL == EL3 then
        MPAM1_EL1 = val;
                  

MPAM1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x900] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                MPAM1_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            MPAM1_EL1 = val;
        else
            UNDEFINED;
                  

ICH_HCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x4C0] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            ICH_HCR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICH_HCR_EL2 = val[31:0];
                  

IFSR32_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        IFSR32_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        IFSR32_EL2 = val[31:0];
                  

APIAKeyHi_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APIAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIAKeyHi_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIAKeyHi_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APIAKeyHi_EL1 = val;
                  

CPTR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        CPTR_EL3 = val[31:0];
                  

ICH_VMCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x4C8] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            ICH_VMCR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICH_VMCR_EL2 = val[31:0];
                  

CPTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            CPTR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CPTR_EL2 = val[31:0];
                  

CPTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && CPTR_EL2.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.CPACR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x100] = val;
        else
            CPACR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TCPAC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            CPTR_EL2 = val[31:0];
        else
            CPACR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CPACR_EL1 = val[31:0];
                  

CNTHPS_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            CNTHPS_CVAL_EL2 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            CNTHPS_CVAL_EL2 = val;
                  

CNTHPS_CVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CVAL_EL2 = val;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_CVAL_EL2 = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x178] = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CVAL_EL2 = val;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_CVAL_EL2 = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL3 then
        CNTP_CVAL_EL0 = val;
                  

VSTTBR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x030] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            VSTTBR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            VSTTBR_EL2 = val;
                  

HSTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x080] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        HSTR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        HSTR_EL2 = val[31:0];
                  

APGAKeyLo_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APGAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APGAKeyLo_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APGAKeyLo_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APGAKeyLo_EL1 = val;
                  

SPSR_irq_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SPSR_irq = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_irq = val[31:0];
                  

RMR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL1 && IsHighestEL(EL1) then
        RMR_EL1 = val[31:0];
    else
        UNDEFINED;
                  

TTBR0_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        TTBR0_EL3 = val;
                  

TRFCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            TRFCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TRFCR_EL2 = val;
                  

TRFCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.TRFCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TTRF == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x880] = val;
        else
            TRFCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TTRF == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            TRFCR_EL2 = val;
        else
            TRFCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TRFCR_EL1 = val;
                  

TTBR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TTBR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x200] = val;
        else
            TTBR0_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TTBR0_EL2 = val;
        else
            TTBR0_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TTBR0_EL1 = val;
                  

TTBR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x200] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TTBR0_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            TTBR0_EL1 = val;
        else
            UNDEFINED;
                  

S3_op1_Cn_Cm_op2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.TIDCP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            IMPLEMENTATION_DEFINED "";
    else
        IMPLEMENTATION_DEFINED "";
                  

RMR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL3 && IsHighestEL(EL3) then
        RMR_EL3 = val[31:0];
    else
        UNDEFINED;
                  

SCTLR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SCTLR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        SCTLR_EL2 = val;
                  

SCTLR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.SCTLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x110] = val;
        else
            SCTLR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            SCTLR_EL2 = val;
        else
            SCTLR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        SCTLR_EL1 = val;
                  

PAN_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        PSTATE.PAN = val[22];
    elsif PSTATE.EL == EL2 then
        PSTATE.PAN = val[22];
    elsif PSTATE.EL == EL3 then
        PSTATE.PAN = val[22];
                  

SDER32_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        SDER32_EL3 = val[31:0];
                  

VSTCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x048] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            VSTCR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            VSTCR_EL2 = val[31:0];
                  

MPAMVPM6_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x970] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM6_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM6_EL2 = val;
                  

HFGWTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1C0] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            HFGWTR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HFGWTR_EL2 = val;
                  

SDER32_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            SDER32_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        SDER32_EL2 = val[31:0];
                  

PMXEVCNTR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMXEVCNTR_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMXEVCNTR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMXEVCNTR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMXEVCNTR_EL0 = val[31:0];
                  

SCTLR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        SCTLR_EL3 = val;
                  

RMR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL1 && EL2Enabled() && IsHighestEL(EL2) && HCR_EL2.NV == '1' then
        AArch64.SystemAccessTrap(EL2, 0x18);
    elsif PSTATE.EL == EL2 && IsHighestEL(EL2) then
        RMR_EL2 = val[31:0];
    else
        UNDEFINED;
                  

CNTV_CVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CVAL_EL2 = val;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_CVAL_EL2 = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x168] = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CVAL_EL2 = val;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_CVAL_EL2 = val;
        else
            CNTV_CVAL_EL0 = val;
    elsif PSTATE.EL == EL3 then
        CNTV_CVAL_EL0 = val;
                  

CNTV_CVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVVCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                NVMem[0x168] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTV_CVAL_EL0 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTV_CVAL_EL0 = val;
        else
            UNDEFINED;
                  

ERRSELR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERRSELR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERRSELR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERRSELR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERRSELR_EL1 = val;
                  

PMSWINC_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.[SW,EN] == '00' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSWINC_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSWINC_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSWINC_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSWINC_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSWINC_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMSWINC_EL0 = val[31:0];
                  

ICC_PMR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_PMR_EL1 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_PMR_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_PMR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_PMR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_PMR_EL1 = val[31:0];
                  

ICC_CTLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_CTLR_EL1 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_CTLR_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_CTLR_EL1 = val[31:0];
            else
                ICC_CTLR_EL1 = val[31:0];
        else
            ICC_CTLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_CTLR_EL1 = val[31:0];
            else
                ICC_CTLR_EL1 = val[31:0];
        else
            ICC_CTLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_CTLR_EL1 = val[31:0];
            else
                ICC_CTLR_EL1 = val[31:0];
                  

ZCR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            ZCR_EL3 = val;
                  

ERXSTATUS_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXSTATUS_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXSTATUS_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXSTATUS_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXSTATUS_EL1 = val;
                  

TTBR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TTBR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x210] = val;
        else
            TTBR1_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TTBR1_EL2 = val;
        else
            TTBR1_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TTBR1_EL1 = val;
                  

TTBR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x210] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TTBR1_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            TTBR1_EL1 = val;
        else
            UNDEFINED;
                  

CNTVOFF_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x060] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTVOFF_EL2 = val;
    elsif PSTATE.EL == EL3 then
        CNTVOFF_EL2 = val;
                  

MDCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MDCR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        MDCR_EL2 = val[31:0];
                  

MPAMVPM7_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x978] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM7_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM7_EL2 = val;
                  

FAR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            FAR_EL1 = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        FAR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        FAR_EL2 = val;
                  

FAR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.FAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x220] = val;
        else
            FAR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            FAR_EL2 = val;
        else
            FAR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        FAR_EL1 = val;
                  

TFSR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                TFSR_EL1 = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            TFSR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TFSR_EL2 = val;
                  

TFSR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x190] = val;
        else
            TFSR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            TFSR_EL2 = val;
        else
            TFSR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TFSR_EL1 = val;
                  

TFSR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        TFSR_EL3 = val;
                  

FAR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        FAR_EL3 = val;
                  

HFGITR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1C8] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            HFGITR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HFGITR_EL2 = val;
                  

ERXADDR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXADDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXADDR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXADDR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXADDR_EL1 = val;
                  

MDCR_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        MDCR_EL3 = val[31:0];
                  

PMSCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x828] = val;
        else
            PMSCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            PMSCR_EL2 = val;
        else
            PMSCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSCR_EL1 = val;
                  

PMSCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x828] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                PMSCR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            PMSCR_EL1 = val;
        else
            UNDEFINED;
                  

ZCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            ZCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            ZCR_EL2 = val;
                  

ZCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x1E0] = val;
        else
            ZCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif HCR_EL2.E2H == '1' then
            ZCR_EL2 = val;
        else
            ZCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            ZCR_EL1 = val;
                  

ERXCTLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TERR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXCTLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXCTLR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TERR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXCTLR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXCTLR_EL1 = val;
                  

DBGCLAIMSET_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGCLAIM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGCLAIMSET_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGCLAIMSET_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        DBGCLAIMSET_EL1 = val[31:0];
                  

DBGVCR32_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            DBGVCR32_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        DBGVCR32_EL2 = val[31:0];
                  

TTBR1_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        TTBR1_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TTBR1_EL2 = val;
                  

TTBR1_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TTBR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x210] = val;
        else
            TTBR1_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            TTBR1_EL2 = val;
        else
            TTBR1_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TTBR1_EL1 = val;
                  

FAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.FAR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x220] = val;
        else
            FAR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            FAR_EL2 = val;
        else
            FAR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        FAR_EL1 = val;
                  

FAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x220] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            FAR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            FAR_EL1 = val;
        else
            UNDEFINED;
                  

FAR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            FAR_EL1 = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        FAR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        FAR_EL2 = val;
                  

MPAM0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAM2_EL2.TRAPMPAM0EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            MPAM0_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAM0_EL1 = val;
    elsif PSTATE.EL == EL3 then
        MPAM0_EL1 = val;
                  

CNTP_CVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CVAL_EL2 = val;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_CVAL_EL2 = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x178] = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CVAL_EL2 = val;
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_CVAL_EL2 = val;
        else
            CNTP_CVAL_EL0 = val;
    elsif PSTATE.EL == EL3 then
        CNTP_CVAL_EL0 = val;
                  

CNTP_CVAL_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            if EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1NVPCT == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                NVMem[0x178] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTP_CVAL_EL0 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CNTP_CVAL_EL0 = val;
        else
            UNDEFINED;
                  

TFSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1] == '01' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x190] = val;
        else
            TFSR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            TFSR_EL2 = val;
        else
            TFSR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TFSR_EL1 = val;
                  

TFSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x190] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                TFSR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            TFSR_EL1 = val;
        else
            UNDEFINED;
                  

TFSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                TFSR_EL1 = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            TFSR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        TFSR_EL2 = val;
                  

ICH_AP1R_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x4A0+8*UInt(op2[1:0])] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            ICH_AP1R_EL2[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICH_AP1R_EL2[UInt(op2[1:0])] = val;
                  

APDAKeyLo_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APDAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDAKeyLo_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDAKeyLo_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APDAKeyLo_EL1 = val;
                  

TFSRE0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.ATA == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            TFSRE0_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.ATA == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            TFSRE0_EL1 = val;
    elsif PSTATE.EL == EL3 then
        TFSRE0_EL1 = val;
                  

AMCR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMCR_EL0 = val;
    else
        UNDEFINED;
                  

PMINTENSET_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMINTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMINTENSET_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMINTENSET_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMINTENSET_EL1 = val[31:0];
                  

PMSCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMSCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        PMSCR_EL2 = val;
                  

PMSCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMSCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x828] = val;
        else
            PMSCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            PMSCR_EL2 = val;
        else
            PMSCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMSCR_EL1 = val;
                  

ZCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x1E0] = val;
        else
            ZCR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x19);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        elsif HCR_EL2.E2H == '1' then
            ZCR_EL2 = val;
        else
            ZCR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.EZ == '0' then
            AArch64.SystemAccessTrap(EL3, 0x19);
        else
            ZCR_EL1 = val;
                  

ZCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x1E0] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            if HCR_EL2.E2H == '0' && CPTR_EL2.TZ == '1' then
                AArch64.SystemAccessTrap(EL2, 0x19);
            elsif HCR_EL2.E2H == '1' && CPTR_EL2.ZEN == 'x0' then
                AArch64.SystemAccessTrap(EL2, 0x19);
            elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.EZ == '0' then
                AArch64.SystemAccessTrap(EL3, 0x19);
            else
                ZCR_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            if CPTR_EL3.EZ == '0' then
                AArch64.SystemAccessTrap(EL3, 0x19);
            else
                ZCR_EL1 = val;
        else
            UNDEFINED;
                  

CONTEXTIDR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.CONTEXTIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x108] = val;
        else
            CONTEXTIDR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CONTEXTIDR_EL2 = val[31:0];
        else
            CONTEXTIDR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CONTEXTIDR_EL1 = val[31:0];
                  

CONTEXTIDR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x108] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CONTEXTIDR_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            CONTEXTIDR_EL1 = val[31:0];
        else
            UNDEFINED;
                  

MPAMVPM5_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x968] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM5_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM5_EL2 = val;
                  

LORN_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TLOR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.LORN_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LORN_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.TLOR == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            LORN_EL1 = val;
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.NS == '0' then
            UNDEFINED;
        else
            LORN_EL1 = val;
                  

VSESR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x508] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VSESR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        VSESR_EL2 = val;
                  

AFSR1_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        AFSR1_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        AFSR1_EL2 = val[31:0];
                  

AFSR1_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.AFSR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x130] = val;
        else
            AFSR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AFSR1_EL2 = val[31:0];
        else
            AFSR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        AFSR1_EL1 = val[31:0];
                  

MPAM2_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAM2_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAM2_EL2 = val;
                  

MPAM2_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MPAM2_EL2.TRAPMPAM1EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x900] = val;
        else
            MPAM1_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HCR_EL2.E2H == '1' then
            MPAM2_EL2 = val;
        else
            MPAM1_EL1 = val;
    elsif PSTATE.EL == EL3 then
        MPAM1_EL1 = val;
                  

SP_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        SP_EL2 = val;
                  

APDAKeyHi_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APDAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDAKeyHi_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APDAKeyHi_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APDAKeyHi_EL1 = val;
                  

DBGBVR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGBVRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGBVR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGBVR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGBVR_EL1[UInt(CRm[3:0])] = val;
                  

CNTHVS_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            CNTHVS_CTL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            CNTHVS_CTL_EL2 = val[31:0];
                  

CNTHVS_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CTL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_CTL_EL2 = val[31:0];
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x170] = val;
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_CTL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_CTL_EL2 = val[31:0];
        else
            CNTV_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTV_CTL_EL0 = val[31:0];
                  

ICH_LR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x400+8*UInt(CRm[0]:op2[2:0])] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            ICH_LR_EL2[UInt(CRm[0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICH_LR_EL2[UInt(CRm[0]:op2[2:0])] = val;
                  

PMCNTENSET_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCNTENSET_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCNTEN == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCNTENSET_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCNTENSET_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMCNTENSET_EL0 = val[31:0];
                  

HCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x078] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        HCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HCR_EL2 = val;
                  

AFSR1_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        AFSR1_EL3 = val[31:0];
                  

MDCCINT_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TDCC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MDCCINT_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDCC == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MDCCINT_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        MDCCINT_EL1 = val[31:0];
                  

DBGBCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.DBGBCRn_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGBCR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGBCR_EL1[UInt(CRm[3:0])] = val;
    elsif PSTATE.EL == EL3 then
        if !ELUsingAArch32(EL1) && OSLSR_EL1.OSLK == '0' && HaltingAllowed() && EDSCR.TDA == '1' then
            Halt(DebugHalt_SoftwareAccess);
        else
            DBGBCR_EL1[UInt(CRm[3:0])] = val;
                  

PMUSERENR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMUSERENR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMUSERENR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMUSERENR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMUSERENR_EL0 = val[31:0];
                  

CONTEXTIDR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CONTEXTIDR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CONTEXTIDR_EL2 = val[31:0];
                  

CONTEXTIDR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.CONTEXTIDR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x108] = val;
        else
            CONTEXTIDR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CONTEXTIDR_EL2 = val[31:0];
        else
            CONTEXTIDR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CONTEXTIDR_EL1 = val[31:0];
                  

CNTPS_CVAL_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '0' then
            if SCR_EL3.EEL2 == '1' then
                UNDEFINED;
            elsif SCR_EL3.ST == '0' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                CNTPS_CVAL_EL1 = val;
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        CNTPS_CVAL_EL1 = val;
                  

FPCR_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CPACR_EL1.FPEN != '11' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x00);
            else
                AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CPTR_EL2.FPEN != '11' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPCR = val[31:0];
    elsif PSTATE.EL == EL1 then
        if CPACR_EL1.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL1, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H != '1' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPCR = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPCR = val[31:0];
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPCR = val[31:0];
                  

AFSR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.AFSR1_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x130] = val;
        else
            AFSR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AFSR1_EL2 = val[31:0];
        else
            AFSR1_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        AFSR1_EL1 = val[31:0];
                  

AFSR1_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x130] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AFSR1_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            AFSR1_EL1 = val[31:0];
        else
            UNDEFINED;
                  

CNTHVS_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            CNTHVS_TVAL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            CNTHVS_TVAL_EL2 = val[31:0];
                  

CNTHVS_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_TVAL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_TVAL_EL2 = val[31:0];
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_TVAL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_TVAL_EL2 = val[31:0];
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTV_TVAL_EL0 = val[31:0];
                  

SP_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x240] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SP_EL1 = val;
    elsif PSTATE.EL == EL3 then
        SP_EL1 = val;
                  

SPSR_abt_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SPSR_abt = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_abt = val[31:0];
                  

DISR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.AMO == '1' then
            VDISR_EL2 = val;
        else
            DISR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        DISR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        DISR_EL1 = val;
                  

SP_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if PSTATE.SP == '0' then
            UNDEFINED;
        else
            SP_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if PSTATE.SP == '0' then
            UNDEFINED;
        else
            SP_EL0 = val;
    elsif PSTATE.EL == EL3 then
        if PSTATE.SP == '0' then
            UNDEFINED;
        else
            SP_EL0 = val;
                  

CNTHP_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHP_TVAL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTHP_TVAL_EL2 = val[31:0];
                  

CNTHP_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_TVAL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_TVAL_EL2 = val[31:0];
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_TVAL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_TVAL_EL2 = val[31:0];
        else
            CNTP_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTP_TVAL_EL0 = val[31:0];
                  

SPSel_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        PSTATE.SP = val[0];
    elsif PSTATE.EL == EL2 then
        PSTATE.SP = val[0];
    elsif PSTATE.EL == EL3 then
        PSTATE.SP = val[0];
                  

AFSR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.AFSR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x128] = val;
        else
            AFSR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AFSR0_EL2 = val[31:0];
        else
            AFSR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        AFSR0_EL1 = val[31:0];
                  

AFSR0_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV1,NV] == '101' then
            NVMem[0x128] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AFSR0_EL1 = val[31:0];
        else
            UNDEFINED;
    elsif PSTATE.EL == EL3 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' then
            AFSR0_EL1 = val[31:0];
        else
            UNDEFINED;
                  

PMCCNTR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCCNTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCCNTR_EL0 = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCCNTR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCCNTR_EL0 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCCNTR_EL0 = val;
    elsif PSTATE.EL == EL3 then
        PMCCNTR_EL0 = val;
                  

VPIDR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x088] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VPIDR_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if !HaveEL(EL2) then
            //no operation
        else
            VPIDR_EL2 = val[31:0];
                  

VNCR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x0B0] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VNCR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        VNCR_EL2 = val;
                  

CSSELR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID2 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TID4 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.CSSELR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CSSELR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        CSSELR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CSSELR_EL1 = val[31:0];
                  

ERXPFGCTL_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FIEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.ERXPFGCTL_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXPFGCTL_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FIEN == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ERXPFGCTL_EL1 = val;
    elsif PSTATE.EL == EL3 then
        ERXPFGCTL_EL1 = val;
                  

MDSCR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.MDSCR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDA] != '00' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x158] = val;
        else
            MDSCR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDA == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MDSCR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        MDSCR_EL1 = val[31:0];
                  

HDFGWTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1D8] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            HDFGWTR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HDFGWTR_EL2 = val;
                  

SPSR_fiq_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        SPSR_fiq = val[31:0];
    elsif PSTATE.EL == EL3 then
        SPSR_fiq = val[31:0];
                  

HAFGRTR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x1E8] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.FGTEn == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            HAFGRTR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        HAFGRTR_EL2 = val;
                  

VTTBR_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x020] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        VTTBR_EL2 = val;
    elsif PSTATE.EL == EL3 then
        VTTBR_EL2 = val;
                  

OSDLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" && HDFGWTR_EL2.OSDLR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.[TDE,TDOSA] != '00' && (boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" || boolean IMPLEMENTATION_DEFINED "Trapped by MDCR_EL2.TDOSA") then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' && (boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" || boolean IMPLEMENTATION_DEFINED "Trapped by MDCR_EL3.TDOSA") then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSDLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TDOSA == '1' && (boolean IMPLEMENTATION_DEFINED "ARMv8.0-DoubleLock" || boolean IMPLEMENTATION_DEFINED "Trapped by MDCR_EL3.TDOSA") then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            OSDLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        OSDLR_EL1 = val[31:0];
                  

PMOVSCLR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMOVSCLR_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMOVS == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMOVSCLR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMOVSCLR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMOVSCLR_EL0 = val[31:0];
                  

CNTHPS_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && SCR_EL3.NS == '1' then
            UNDEFINED;
        else
            CNTHPS_CTL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if SCR_EL3.EEL2 == '0' then
            UNDEFINED;
        else
            CNTHPS_CTL_EL2 = val[31:0];
                  

CNTHPS_CTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0PTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '10' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CTL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHP_CTL_EL2 = val[31:0];
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '0' && CNTHCTL_EL2.EL1PCEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.E2H == '1' && CNTHCTL_EL2.EL1PTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x180] = val;
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHPS_CTL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHP_CTL_EL2 = val[31:0];
        else
            CNTP_CTL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTP_CTL_EL0 = val[31:0];
                  

PMCR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMCR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCR_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMCR_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPMCR == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCR_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMCR_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        PMCR_EL0 = val[31:0];
                  

AFSR0_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        AFSR0_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        AFSR0_EL2 = val[31:0];
                  

AFSR0_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TVM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.AFSR0_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '111' then
            NVMem[0x128] = val;
        else
            AFSR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            AFSR0_EL2 = val[31:0];
        else
            AFSR0_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        AFSR0_EL1 = val[31:0];
                  

UAO_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        PSTATE.UAO = val[23];
    elsif PSTATE.EL == EL2 then
        PSTATE.UAO = val[23];
    elsif PSTATE.EL == EL3 then
        PSTATE.UAO = val[23];
                  

MPAMVPM4_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x960] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
                AArch64.SystemAccessTrap(EL3, 0x18);
            else
                AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && MPAM3_EL3.TRAPLOWER == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            MPAMVPM4_EL2 = val;
    elsif PSTATE.EL == EL3 then
        MPAMVPM4_EL2 = val;
                  

ICV_CTLR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_CTLR_EL1 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_CTLR_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_CTLR_EL1 = val[31:0];
            else
                ICC_CTLR_EL1 = val[31:0];
        else
            ICC_CTLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) then
            if SCR_EL3.NS == '0' then
                ICC_CTLR_EL1 = val[31:0];
            else
                ICC_CTLR_EL1 = val[31:0];
        else
            ICC_CTLR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            if SCR_EL3.NS == '0' then
                ICC_CTLR_EL1 = val[31:0];
            else
                ICC_CTLR_EL1 = val[31:0];
                  

FPEXC32_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '0' && CPTR_EL2.TFP == '1' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HCR_EL2.E2H == '1' && CPTR_EL2.FPEN == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x07);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPEXC32_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if CPTR_EL3.TFP == '1' then
            AArch64.SystemAccessTrap(EL3, 0x07);
        else
            FPEXC32_EL2 = val[31:0];
                  

TPIDRRO_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.TPIDRRO_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            TPIDRRO_EL0 = val;
    elsif PSTATE.EL == EL2 then
        TPIDRRO_EL0 = val;
    elsif PSTATE.EL == EL3 then
        TPIDRRO_EL0 = val;
                  

CNTHV_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHV_TVAL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTHV_TVAL_EL2 = val[31:0];
                  

CNTHV_TVAL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && !(EL2Enabled() && HCR_EL2.[E2H,TGE] == '11') && CNTKCTL_EL1.EL0VTEN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && CNTHCTL_EL2.EL0VTEN == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_TVAL_EL2 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[E2H,TGE] == '11' && SCR_EL3.NS == '1' then
            CNTHV_TVAL_EL2 = val[31:0];
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL1 then
        if !ELUsingAArch32(EL1) && EL2Enabled() && HCR_EL2.[E2H,TGE] != '11' && CNTHCTL_EL2.EL1TVT == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' && SCR_EL3.NS == '0' && boolean IMPLEMENTATION_DEFINED "ARMv8.4-SecEL2" then
            CNTHVS_TVAL_EL2 = val[31:0];
        elsif HCR_EL2.E2H == '1' && SCR_EL3.NS == '1' then
            CNTHV_TVAL_EL2 = val[31:0];
        else
            CNTV_TVAL_EL0 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTV_TVAL_EL0 = val[31:0];
                  

CNTHCTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        CNTHCTL_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTHCTL_EL2 = val[31:0];
                  

CNTHCTL_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        CNTKCTL_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if HCR_EL2.E2H == '1' then
            CNTHCTL_EL2 = val[31:0];
        else
            CNTKCTL_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        CNTKCTL_EL1 = val[31:0];
                  

APGAKeyHi_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APGAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APGAKeyHi_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APGAKeyHi_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APGAKeyHi_EL1 = val;
                  

PMEVCNTR_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        if !ELUsingAArch32(EL1) && PMUSERENR_EL0.EN == '0' then
            if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.TGE == '1' then
                AArch64.SystemAccessTrap(EL2, 0x18);
            else
                AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL1) && HCR_EL2.[E2H,TGE] != '11' && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMEVCNTRn_EL0 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.TPM == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && MDCR_EL3.TPM == '1' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
    elsif PSTATE.EL == EL3 then
        PMEVCNTR_EL0[UInt(CRm[1:0]:op2[2:0])] = val;
                  

DACR32_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        DACR32_EL2 = val[31:0];
    elsif PSTATE.EL == EL3 then
        DACR32_EL2 = val[31:0];
                  

ICV_PMR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if ICC_SRE_EL1.SRE == '0' then
            AArch64.SystemAccessTrap(EL1, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && ICH_HCR_EL2.TC == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.FMO == '1' then
            ICV_PMR_EL1 = val[31:0];
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.IMO == '1' then
            ICV_PMR_EL1 = val[31:0];
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_PMR_EL1 = val[31:0];
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.[IRQ,FIQ] == '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_PMR_EL1 = val[31:0];
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICC_PMR_EL1 = val[31:0];
                  

AMEVCNTR0_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMEVCNTR0_EL0[UInt(CRm[0]:op2[2:0])] = val;
    else
        UNDEFINED;
                  

AMEVCNTR1_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        AMEVCNTR1_EL0[UInt(CRm[0]:op2[2:0])] = val;
    else
        UNDEFINED;
                  

PMBSR_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HDFGWTR_EL2.PMBSR_EL1 == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && MDCR_EL2.E2PB == 'x0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.[NV2,NV1,NV] == '1x1' then
            NVMem[0x820] = val;
        else
            PMBSR_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '0' && MDCR_EL3.NSPB != '01' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.NS == '1' && MDCR_EL3.NSPB != '11' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            PMBSR_EL1 = val;
    elsif PSTATE.EL == EL3 then
        PMBSR_EL1 = val;
                  

CNTFRQ_EL0_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if IsHighestEL(PSTATE.EL) then
        CNTFRQ_EL0 = val[31:0];
    else
        UNDEFINED;
                  

APIAKeyLo_EL1_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && !ELUsingAArch32(EL2) && HCR_EL2.APK == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif EL2Enabled() && !ELUsingAArch32(EL2) && (!HaveEL(EL3) || SCR_EL3.FGTEn == '1') && HFGWTR_EL2.APIAKey == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        elsif HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIAKeyLo_EL1 = val;
    elsif PSTATE.EL == EL2 then
        if HaveEL(EL3) && !ELUsingAArch32(EL3) && SCR_EL3.APK == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            APIAKeyLo_EL1 = val;
    elsif PSTATE.EL == EL3 then
        APIAKeyLo_EL1 = val;
                  

ICH_AP0R_EL2_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        if EL2Enabled() && HCR_EL2.[NV2,NV] == '11' then
            NVMem[0x480+8*UInt(op2[1:0])] = val;
        elsif EL2Enabled() && HCR_EL2.NV == '1' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            UNDEFINED;
    elsif PSTATE.EL == EL2 then
        if ICC_SRE_EL2.SRE == '0' then
            AArch64.SystemAccessTrap(EL2, 0x18);
        else
            ICH_AP0R_EL2[UInt(op2[1:0])] = val;
    elsif PSTATE.EL == EL3 then
        if ICC_SRE_EL3.SRE == '0' then
            AArch64.SystemAccessTrap(EL3, 0x18);
        else
            ICH_AP0R_EL2[UInt(op2[1:0])] = val;
                  

MPAM3_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        MPAM3_EL3 = val;
                  

AFSR0_EL3_reg_write(integer op0, integer op1, integer CRn, integer CRm, integer op2, bits(64) val)
    
    if PSTATE.EL == EL0 then
        UNDEFINED;
    elsif PSTATE.EL == EL1 then
        UNDEFINED;
    elsif PSTATE.EL == EL2 then
        UNDEFINED;
    elsif PSTATE.EL == EL3 then
        AFSR0_EL3 = val[31:0];
                  

bits(64) AArch64.SysRegRead(integer op0, integer op1, integer CRn, integer CRm, integer op2)
    case op0[0:0]:op1[2:0]:CRn[3:0]:CRm[3:0]:op2[2:0] of
        when '111111100010001' return CNTPS_CTL_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010101010' return ERXMISC2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011000000000' return VBAR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000000000' return VBAR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100101000' return CNTHPS_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010000' return CNTHPS_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100011' return ICC_BPR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010000001' return ACTLR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010010' return AMCGCR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010011000' return AMCNTENCLR1_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010001' return ID_ISAR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '00000000xxxx110' return DBGWVR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000010' return CPACR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100010000010' return CPACR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011100001000' return CNTKCTL_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100001000' return CNTKCTL_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100010011' return APDBKeyHi_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000011001' return MVFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010000001' return ACTLR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100101' return ICC_SRE_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110001' return MPAMVPM1_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011110011' return PMOVSSET_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100111' return PMCEID1_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '00000000xxxx111' return DBGWCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100100100' return MPAMIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000000000' return SPSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101000000000' return SPSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000000000' return SPSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000000001' return ELR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101000000001' return ELR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000000001' return ELR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100000' return ICV_IAR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111011000000000' return VBAR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110000110001100' return HDFGRTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001001' return ID_PFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000000000' return VBAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110111000000000' return VBAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011010001' return PMBPTR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010110' return ID_MMFR4_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010101' return AMCNTENSET0_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '111001000000001' return ELR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '111001000000000' return SPSR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000111000' return ID_AA64MMFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010001000' return SCR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '000000010000000' return MDRAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100000010' return CNTVCT_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000101101' return ID_AA64AFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000101000' return ID_AA64DFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111011001100101' return ICC_SRE_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000001' return ACTLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011000' return CNTV_TVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100011000' return CNTV_TVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000100100' return ID_AA64ZFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '001100000001000' return MDCCSR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001011101' return ICH_ELRSR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001001101' return ICC_SRE_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001100' return PMSFCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011000001001' return VDISR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000001001' return VDISR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001100000100' return HPFAR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000000000' return SPSR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000000000' return SPSR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100101' return PMSELR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000000001' return ELR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000000001' return ELR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100100011' return LORC_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100100001' return LOREA_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000111001' return ID_AA64MMFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000101100' return ID_AA64AFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010011000' return ERRIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011010000111' return SCXTNUM_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110111010000111' return SCXTNUM_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011010000010' return TPIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001000' return ID_PFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '000000010001100' return OSLSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010011001' return AMCNTENSET1_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000011110' return ID_MMFR5_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '1000110010010xx' return ICV_AP1R_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '001100000101000' return DBGDTRRX_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100011000' return AMAIR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100011000' return AMAIR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000101001' return ID_AA64DFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011001' return CNTV_CTL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100011001' return CNTV_CTL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010010000' return ESR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010010000' return ESR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '1011111011xxxxx' return PMEVTYPER_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '111001010010000' return ESR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '111010100011000' return AMAIR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000101' return RGSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001011001' return ICH_VTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010000111' return SCXTNUM_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '111011010000010' return TPIDR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100100000000100' return GMID_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001110' return PMSLATFR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100010' return PMCNTENCLR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100011010000100' return TPIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010101011' return ERXMISC3_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011010000111' return SCXTNUM_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011010000111' return SCXTNUM_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '000000000011010' return OSDTRTX_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001101' return PMSEVFR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100011001' return CNTHV_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011001' return CNTHV_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000000000101' return VMPIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000000101' return VMPIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010000' return ID_ISAR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '000000010100100' return DBGPRCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100011000' return AMAIR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110110100011000' return AMAIR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001000011' return ICC_BPR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010010000' return ESR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101010010000' return ESR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010010000' return ESR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010100' return AMCNTENCLR0_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100001010' return APIBKeyLo_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000011000' return MVFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000010110' return SSBS_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110000' return MPAMVPM0_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100000001' return CNTPCT_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100100000' return MPAMHCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010000' return CNTP_TVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100010000' return CNTP_TVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '1000110010010xx' return ICC_AP1R_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010000010' return TPIDR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '111011010000111' return SCXTNUM_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001000000' return ICV_IAR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011010000' return PMBLIMITR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100110' return PMCEID0_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000100001' return ID_AA64PFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000101000' return DSPSR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100100000000010' return CCSIDR2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100010000' return MAIR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110110100010000' return MAIR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000110' return GCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000101001' return DLR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001010' return ID_DFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100110' return ICV_IGRPEN0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100100000000000' return CCSIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000011100' return ID_PFR2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100110' return ICC_IGRPEN0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000000001' return RVBAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100001011' return APIBKeyHi_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010011' return AMUSERENR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001000000' return ICC_IAR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100010' return ICC_HPPIR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '000000000000010' return OSDTRRX_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010010' return ID_ISAR2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100010000' return MAIR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100010000' return MAIR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '000001111001110' return DBGCLAIMCLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011101001' return PMXEVTYPER_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '111011000000001' return RVBAR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000010101' return DIT_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100010010' return CNTHP_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010010' return CNTHP_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010101001' return ERXMISC1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011110010' return PMINTENCLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011000000001' return RVBAR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010110' return AMCG1IDR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001000011' return ICV_BPR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111111100010000' return CNTPS_TVAL_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '10111101011xxxx' return AMEVTYPER0_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110010' return MPAMVPM2_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000010000' return NZCV_reg_read(op0, op1, CRn, CRm, op2);
        when '111010100010000' return MAIR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000011010' return SPSR_und_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000011010' return MVFR2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100100010' return CNTHVS_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011010' return CNTHVS_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '1000110010001xx' return ICV_AP0R_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '11001101100xxxx' return AMEVCNTVOFF0_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100000' return ICC_IAR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000001000' return ISR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001000010' return ICC_HPPIR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '11001101101xxxx' return AMEVCNTVOFF1_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100000010' return TCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100100000010' return TCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010011' return ID_ISAR3_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010101000' return ERXMISC0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111011001100111' return ICC_IGRPEN1_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100100001' return MPAMVPMV_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000010111' return TCO_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100011' return ICV_BPR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010001' return CNTP_CTL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100010001' return CNTP_CTL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000010001' return DAIF_reg_read(op0, op1, CRn, CRm, op2);
        when '101111101111111' return PMCCFILTR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110011' return MPAMVPM3_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101100000000111' return DCZID_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001111' return HACR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101100000000001' return CTR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001011' return ID_AFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001110100000' return PAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000000101' return MPIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100000010' return TCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100000010' return TCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100100000000111' return AIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000100000' return ID_AA64PFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100010010' return APDBKeyLo_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100111' return ICV_IGRPEN1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100001010' return VTCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '000000000110010' return OSECCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000111010' return ID_AA64MMFR2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000011101' return ID_DFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100010001' return CNTHP_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010001' return CNTHP_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '1000110010001xx' return ICC_AP0R_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '001100000100000' return DBGDTR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100111' return ICC_IGRPEN1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100000110' return CNTPOFF_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001100' return HFGRTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '111000100000010' return TCR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000100001' return FPSR_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100110' return ERXPFGCDN_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001011' return PMSIRR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100011010' return CNTHV_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011010' return CNTHV_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100000000' return TTBR0_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100000000' return TTBR0_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100100000' return LORSA_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111011001100100' return ICC_CTLR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010010001' return TRFCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100010010001' return TRFCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001010' return PMSICR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000000' return SCTLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100010000000' return SCTLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100101000' return MPAM1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110110100101000' return MPAM1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001011000' return ICH_HCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010000001' return IFSR32_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100001001' return APIAKeyHi_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010001010' return CPTR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001011111' return ICH_VMCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011110110' return PMMIR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100000' return ERXFR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001010' return CPTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000010' return CPTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100101010' return CNTHPS_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010010' return CNTHPS_CVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100110000' return VSTTBR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001011' return HSTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100011000' return APGAKeyLo_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000011000' return SPSR_irq_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001111' return ID_MMFR3_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000000010' return RMR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111000100000000' return TTBR0_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010010001' return TRFCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010010001' return TRFCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100000000' return TTBR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100100000000' return TTBR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '1xxx1x11xxxxxxx' return S3_op1_Cn_Cm_op2_reg_read(op0, op1, CRn, CRm, op2);
        when '111011000000010' return RMR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010001' return AMCFGR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100100111' return LORID_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010000000' return SCTLR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010000000' return SCTLR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000010011' return PAN_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010001001' return SDER32_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100110010' return VSTCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110110' return MPAMVPM6_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '000001111110110' return DBGAUTHSTATUS_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100000110' return CNTVCTSS_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010111' return ID_ISAR6_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100010' return ICV_HPPIR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000000110' return REVIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001101' return HFGWTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010011001' return SDER32_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011101010' return PMXEVCNTR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010000000' return SCTLR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110011000000010' return RMR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011010' return CNTV_CVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100011010' return CNTV_CVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010011001' return ERRSELR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001011011' return ICC_RPR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000110000' return ICC_PMR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100100' return ICC_CTLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010010000' return ZCR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100010' return ERXSTATUS_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100000001' return TTBR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100100000001' return TTBR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100000011' return CNTVOFF_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001001' return MDCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110111' return MPAMVPM7_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001100000000' return FAR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001100000000' return FAR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010110000' return TFSR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010110000' return TFSR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001000010' return ICV_HPPIR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111001010110000' return TFSR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '111001100000000' return FAR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001110' return HFGITR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100011' return ERXADDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111000010011001' return MDCR_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001000' return PMSCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110110011001000' return PMSCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010010000' return ZCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010010000' return ZCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100001' return ERXCTLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '000001111000110' return DBGCLAIMSET_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '010000000111000' return DBGVCR32_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100000001' return TTBR1_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100000001' return TTBR1_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011010111' return PMBIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001100000000' return FAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101100000000' return FAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001100000000' return FAR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100101001' return MPAM0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010010' return CNTP_CVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110111100010010' return CNTP_CVAL_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010110000' return TFSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101010110000' return TFSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010110000' return TFSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '1100110010010xx' return ICH_AP1R_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100010000' return APDAKeyLo_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000010010' return CurrentEL_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010110001' return TFSRE0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010010000' return AMCR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011110001' return PMINTENSET_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110010011001000' return PMSCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001000' return PMSCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000010010000' return ZCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110100010010000' return ZCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001110' return ID_MMFR2_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011010000001' return CONTEXTIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110111010000001' return CONTEXTIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110101' return MPAMVPM5_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000000000' return MIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100100010' return LORN_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010010011' return VSESR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010001001' return AFSR1_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010001001' return AFSR1_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100101000' return MPAM2_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010100101000' return MPAM2_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001011011' return ICH_EISR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '111001000001000' return SP_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100010001' return APDAKeyHi_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '00000000xxxx100' return DBGBVR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100100001' return CNTHVS_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011001' return CNTHVS_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '11001100110xxxx' return ICH_LR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100001' return PMCNTENSET_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110000010001000' return HCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '111001010001001' return AFSR1_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010101' return ID_ISAR5_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000110001' return ID_AA64ISAR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '000000000010000' return MDCCINT_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '00000000xxxx101' return DBGBCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011110000' return PMUSERENR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110011010000001' return CONTEXTIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011010000001' return CONTEXTIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '111111100010010' return CNTPS_CVAL_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101101000100000' return FPCR_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010001001' return AFSR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101010001001' return AFSR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100000101' return CNTPCTSS_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100100000' return CNTHVS_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011000' return CNTHVS_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000001000' return SP_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000011001' return SPSR_abt_reg_read(op0, op1, CRn, CRm, op2);
        when '100100000000001' return CLIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101100100100000' return RNDR_reg_read(op0, op1, CRn, CRm, op2);
        when '100011000001001' return DISR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001100' return ID_MMFR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000001000' return SP_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100010000' return CNTHP_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010000' return CNTHP_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000010000' return SPSel_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010001000' return AFSR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110101010001000' return AFSR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101100100100001' return RNDRRS_reg_read(op0, op1, CRn, CRm, op2);
        when '110011001011010' return ICH_MISR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011001111' return PMSIDR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011101000' return PMCCNTR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110000000000000' return VPIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000000000' return VPIDR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100010000' return VNCR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101000000000000' return CSSELR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100101' return ERXPFGCTL_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '000000000010010' return MDSCR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110000110001101' return HDFGWTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000001101' return ID_MMFR1_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001000011011' return SPSR_fiq_reg_read(op0, op1, CRn, CRm, op2);
        when '110000110001110' return HAFGRTR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110000100001000' return VTTBR_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '000000010011100' return OSDLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100011' return PMOVSCLR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100101001' return CNTHPS_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100010001' return CNTHPS_CTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101110011100000' return PMCR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010001000' return AFSR0_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010001000' return AFSR0_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000010100' return UAO_reg_read(op0, op1, CRn, CRm, op2);
        when '100001010100100' return ERXPFGF_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110010100110100' return MPAMVPM4_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001100100' return ICV_CTLR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '110001010011000' return FPEXC32_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111010000011' return TPIDRRO_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100011000' return CNTHV_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100011000' return CNTHV_TVAL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '110011100001000' return CNTHCTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100011100001000' return CNTHCTL_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100011001' return APGAKeyHi_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '1011111010xxxxx' return PMEVCNTR_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '110000110000000' return DACR32_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '100001000110000' return ICV_PMR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100011001011011' return ICV_RPR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100010011010011' return PMBSR_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '101111100000000' return CNTFRQ_EL0_reg_read(op0, op1, CRn, CRm, op2);
        when '100000100001000' return APIAKeyLo_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000110000' return ID_AA64ISAR0_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '1100110010000xx' return ICH_AP0R_EL2_reg_read(op0, op1, CRn, CRm, op2);
        when '111010100101000' return MPAM3_EL3_reg_read(op0, op1, CRn, CRm, op2);
        when '100000000010100' return ID_ISAR4_EL1_reg_read(op0, op1, CRn, CRm, op2);
        when '111001010001000' return AFSR0_EL3_reg_read(op0, op1, CRn, CRm, op2);
        otherwise Unreachable();

AArch64.SysRegWrite(integer op0, integer op1, integer CRn, integer CRm, integer op2, bit(64) val)
    case op0[0:0]:op1[2:0]:CRn[3:0]:CRm[3:0]:op2[2:0] of
        when '111111100010001' CNTPS_CTL_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010101010' ERXMISC2_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011000000000' VBAR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011000000000' VBAR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100001' ICC_EOIR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001011001' ICC_DIR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100101000' CNTHPS_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010000' CNTHPS_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100011' ICC_BPR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010000001' ACTLR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010011000' AMCNTENCLR1_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '00000000xxxx110' DBGWVR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000010' CPACR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100010000010' CPACR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011100001000' CNTKCTL_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100001000' CNTKCTL_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100010011' APDBKeyHi_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001011111' ICC_SGI0R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001011101' ICC_SGI1R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010000001' ACTLR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100101' ICC_SRE_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110001' MPAMVPM1_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011110011' PMOVSSET_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '00000000xxxx111' DBGWCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000000000' SPSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101000000000' SPSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000000000' SPSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000000001' ELR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101000000001' ELR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000000001' ELR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011000000000' VBAR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000110001100' HDFGRTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011000000000' VBAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111000000000' VBAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011010001' PMBPTR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010010101' AMCNTENSET0_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001000000001' ELR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001000000000' SPSR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010001000' SCR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011001100101' ICC_SRE_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000001' ACTLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011000' CNTV_TVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100011000' CNTV_TVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100001' ICV_EOIR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011001001101' ICC_SRE_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001100' PMSFCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011000001001' VDISR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011000001001' VDISR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001100000100' HPFAR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000000000' SPSR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000000000' SPSR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011100101' PMSELR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000000001' ELR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000000001' ELR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100100011' LORC_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100100001' LOREA_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011010000111' SCXTNUM_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111010000111' SCXTNUM_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011010000010' TPIDR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010011001' AMCNTENSET1_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001011110' ICC_ASGI1R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1000110010010xx' ICV_AP1R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100011000' AMAIR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100011000' AMAIR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011001' CNTV_CTL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100011001' CNTV_CTL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010010000' ESR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010010000' ESR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1011111011xxxxx' PMEVTYPER_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001010010000' ESR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111010100011000' AMAIR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000101' RGSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001000001' ICV_EOIR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010000111' SCXTNUM_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011010000010' TPIDR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001110' PMSLATFR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011100010' PMCNTENCLR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011010000100' TPIDR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010101011' ERXMISC3_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011010000111' SCXTNUM_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011010000111' SCXTNUM_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000000011010' OSDTRTX_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001101' PMSEVFR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001000001' ICC_EOIR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100011001' CNTHV_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011001' CNTHV_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000000000101' VMPIDR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000010100100' DBGPRCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100011000' AMAIR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110110100011000' AMAIR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001000011' ICC_BPR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010010000' ESR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101010010000' ESR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010010000' ESR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010010100' AMCNTENCLR0_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100001010' APIBKeyLo_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000010110' SSBS_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110000' MPAMVPM0_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100100000' MPAMHCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010000' CNTP_TVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100010000' CNTP_TVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1000110010010xx' ICC_AP1R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010000010' TPIDR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011010000111' SCXTNUM_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011010000' PMBLIMITR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000101000' DSPSR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100010000' MAIR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110110100010000' MAIR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000110' GCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000101001' DLR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100110' ICV_IGRPEN0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '001100000101000' DBGDTRTX_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100110' ICC_IGRPEN0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100001011' APIBKeyHi_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010010011' AMUSERENR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000000000010' OSDTRRX_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100010000' MAIR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100010000' MAIR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000001111001110' DBGCLAIMCLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011101001' PMXEVTYPER_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000010101' DIT_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100010010' CNTHP_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010010' CNTHP_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010101001' ERXMISC1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011110010' PMINTENCLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001000011' ICV_BPR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111111100010000' CNTPS_TVAL_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '10111101111xxxx' AMEVTYPER1_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110010' MPAMVPM2_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000010000' NZCV_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111010100010000' MAIR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000011010' SPSR_und_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100100010' CNTHVS_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011010' CNTHVS_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1000110010001xx' ICV_AP0R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '11001101100xxxx' AMEVCNTVOFF0_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '11001101101xxxx' AMEVCNTVOFF1_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100000010' TCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100100000010' TCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010101000' ERXMISC0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001011001' ICV_DIR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011001100111' ICC_IGRPEN1_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100100001' MPAMVPMV_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000010111' TCO_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100011' ICV_BPR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010001' CNTP_CTL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100010001' CNTP_CTL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000010001' DAIF_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111101111111' PMCCFILTR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110011' MPAMVPM3_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000010000100' OSLAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001111' HACR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001110100000' PAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100000010' TCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100000010' TCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100010010' APDBKeyLo_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100111' ICV_IGRPEN1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100001010' VTCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000000110010' OSECCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100010001' CNTHP_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010001' CNTHP_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1000110010001xx' ICC_AP0R_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '001100000100000' DBGDTR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100111' ICC_IGRPEN1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100000110' CNTPOFF_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001100' HFGRTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000100000010' TCR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000100001' FPSR_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010100110' ERXPFGCDN_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001011' PMSIRR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100011010' CNTHV_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011010' CNTHV_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100000000' TTBR0_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100000000' TTBR0_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100100000' LORSA_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011001100100' ICC_CTLR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010010001' TRFCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100010010001' TRFCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001010' PMSICR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000000' SCTLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100010000000' SCTLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100101000' MPAM1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110110100101000' MPAM1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011001011000' ICH_HCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010000001' IFSR32_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100001001' APIAKeyHi_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010001010' CPTR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011001011111' ICH_VMCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001010' CPTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000010' CPTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100101010' CNTHPS_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010010' CNTHPS_CVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100110000' VSTTBR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001011' HSTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100011000' APGAKeyLo_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000011000' SPSR_irq_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011000000010' RMR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000100000000' TTBR0_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010010001' TRFCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010010001' TRFCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100000000' TTBR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100100000000' TTBR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1xxx1x11xxxxxxx' S3_op1_Cn_Cm_op2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111011000000010' RMR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010000000' SCTLR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010000000' SCTLR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000010011' PAN_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010001001' SDER32_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100110010' VSTCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110110' MPAMVPM6_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001101' HFGWTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010011001' SDER32_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011101010' PMXEVCNTR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010000000' SCTLR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011000000010' RMR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011010' CNTV_CVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100011010' CNTV_CVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010011001' ERRSELR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011100100' PMSWINC_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000110000' ICC_PMR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100100' ICC_CTLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010010000' ZCR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010100010' ERXSTATUS_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100000001' TTBR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100100000001' TTBR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100000011' CNTVOFF_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001001' MDCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110111' MPAMVPM7_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001100000000' FAR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001100000000' FAR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010110000' TFSR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010110000' TFSR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001010110000' TFSR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001100000000' FAR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001110' HFGITR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010100011' ERXADDR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111000010011001' MDCR_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001000' PMSCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110110011001000' PMSCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010010000' ZCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010010000' ZCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010100001' ERXCTLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000001111000110' DBGCLAIMSET_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '010000000111000' DBGVCR32_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100000001' TTBR1_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100000001' TTBR1_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001100000000' FAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101100000000' FAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001100000000' FAR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100101001' MPAM0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010010' CNTP_CVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111100010010' CNTP_CVAL_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010110000' TFSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101010110000' TFSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010110000' TFSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1100110010010xx' ICH_AP1R_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100010000' APDAKeyLo_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010110001' TFSRE0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010010000' AMCR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011110001' PMINTENSET_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010011001000' PMSCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011001000' PMSCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000010010000' ZCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110100010010000' ZCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011010000001' CONTEXTIDR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110111010000001' CONTEXTIDR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110101' MPAMVPM5_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100100010' LORN_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010010011' VSESR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010001001' AFSR1_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010001001' AFSR1_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100101000' MPAM2_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010100101000' MPAM2_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001000001000' SP_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100010001' APDAKeyHi_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '00000000xxxx100' DBGBVR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100100001' CNTHVS_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011001' CNTHVS_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '11001100110xxxx' ICH_LR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011100001' PMCNTENSET_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000010001000' HCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001010001001' AFSR1_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000000010000' MDCCINT_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '00000000xxxx101' DBGBCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011110000' PMUSERENR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011010000001' CONTEXTIDR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011010000001' CONTEXTIDR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111111100010010' CNTPS_CVAL_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101101000100000' FPCR_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010001001' AFSR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101010001001' AFSR1_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100100000' CNTHVS_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011000' CNTHVS_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000001000' SP_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000011001' SPSR_abt_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011000001001' DISR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000001000' SP_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100010000' CNTHP_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010000' CNTHP_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000010000' SPSel_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010001000' AFSR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110101010001000' AFSR0_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011101000' PMCCNTR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000000000000' VPIDR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100010000' VNCR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101000000000000' CSSELR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010100101' ERXPFGCTL_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000000010010' MDSCR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000110001101' HDFGWTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001000011011' SPSR_fiq_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000110001110' HAFGRTR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000100001000' VTTBR_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '000000010011100' OSDLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011100011' PMOVSCLR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100101001' CNTHPS_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100010001' CNTHPS_CTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101110011100000' PMCR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010001000' AFSR0_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001010001000' AFSR0_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000010100' UAO_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110010100110100' MPAMVPM4_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011001100100' ICV_CTLR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110001010011000' FPEXC32_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111010000011' TPIDRRO_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100011000' CNTHV_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100011000' CNTHV_TVAL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110011100001000' CNTHCTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100011100001000' CNTHCTL_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100011001' APGAKeyHi_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1011111010xxxxx' PMEVCNTR_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '110000110000000' DACR32_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100001000110000' ICV_PMR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '10111101010xxxx' AMEVCNTR0_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '10111101110xxxx' AMEVCNTR1_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100010011010011' PMBSR_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '101111100000000' CNTFRQ_EL0_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '100000100001000' APIAKeyLo_EL1_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '1100110010000xx' ICH_AP0R_EL2_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111010100101000' MPAM3_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        when '111001010001000' AFSR0_EL3_reg_write(op0, op1, CRn, CRm, op2, val[63:0]);
        otherwise Unreachable();
