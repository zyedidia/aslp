integer test_fun(integer x)
    if (x > 0) then
        return x;
        assert x > 0;
    else
        integer y = x;
        integer z = x;
    return x + x;

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
        boolean something;
        //integer d = 1 + UInt(Rd);
        //if (something) then
        //    d = d + d;
        //else
        //    d = 2;

        //bits(64) aaa = ZeroExtend('11');

        bits(64) left = Zeros();
        bits(64) right = Zeros();

        if (left == right) then
            something = TRUE;
        else
            something = FALSE;

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
        __opcode '0001 xxxx  xxxx xxxx  xxxx xxxx xxxxxxxx'
        __guard TRUE
        __decode


    __execute
        integer N = 100;
        integer N2;
        boolean a;
        bits(2) x;
        if (a) then 
            integer N = 102;
        else 
            integer N = 102;
        
        integer NN = test_fun(N2);

__decode TEST
    case (28 +: 4) of
        when (_) => __encoding test
