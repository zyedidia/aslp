__instruction test
    __encoding test
        __instruction_set TEST
        __field Ra 24 +: 4
        __field Rb 20 +: 4
        __field Rc 16 +: 4
        __field Rd 12 +: 4
        __field Re 8 +: 4
        __field Rf 0 +: 1
        //__opcode 'xxx01011 xx0xxxxx xxxxxxxx xxxxxxxx'
        //             a     b    c     d    e    f1-8
        __opcode '0000 xxxx  xxxx xxxx  xxxx xxxx xxxxxxxx'
        __guard TRUE
        __decode


    __execute
        integer d = 2 + UInt(Rd);
        d = if (TRUE) then (d + d) else (-d);


__decode TEST
    case (28 +: 4) of
        when ('0000') => __encoding test
