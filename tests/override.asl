bits(64) AArch64.BranchAddr(bits(64) vaddress)
    return vaddress;


AArch64.CheckFPAdvSIMDEnabled()
    return;

integer ImplementedSVEVectorLength(integer nbits)
    return 128;

boolean IsSVEEnabled(bits(2) el)
    return FALSE;

boolean sle_bits(bits(N) x, bits(N) y)
    integer xn = SInt(x);
    integer yn = SInt(y);
    return xn <= yn;

boolean slt_bits(bits(N) x, bits(N) y)
    integer xn = SInt(x);
    integer yn = SInt(y);
    return xn < yn;

bits(N) neg_bits(bits(N) x)
    return (NOT x) + ZeroExtend('1', N);
