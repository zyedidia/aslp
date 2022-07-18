__instruction test
    __encoding test
        __instruction_set TEST
        __field Ra 24 +: 4
        __field Rb 20 +: 4
        __field Rc 16 +: 4
        __field Rd 12 +: 4
        __field Re 8 +: 4
        __field Rf 0 +: 0
        //__opcode 'xxx01011 xx0xxxxx xxxxxxxx xxxxxxxx'
        //             a     b    c     d    e    f1-8
        __opcode '0000 xxxx  xxxx xxxx  xxxx xxxx xxxxxxxx'
        __guard TRUE
        __decode
            // integer d = UInt(Rd);
            // integer n = UInt(Rn);
            // integer m = UInt(Rm);

            // integer datasize = if sf == '1' then 64 else 32;
            // boolean sub_op = (op == '1');
            // boolean setflags = (S == '1');
            // 
            // if shift == '11' then UNDEFINED;
            // if sf == '0' && imm6[5] == '1' then UNDEFINED;
            // 
            // ShiftType shift_type = DecodeShift(shift);
            // integer shift_amount = UInt(imm6);

    __execute
        integer d = UInt(Rd);
        bits(64) x = ZeroExtend(Ra);
        bits(64) y = X[d];

        
        X[d] = y + 1;

__decode TEST
    case (28 +: 4) of
        when ('0000') => __encoding test

