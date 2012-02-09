#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

%%{
    machine ITCHv41;
    write data;

    # Common fields for both Ordered Executed message types
    nanoseconds            = any{4} >{ nanos    = __builtin_bswap32(*(const uint32_t*)(p)); };
    orderExecutedShares4   = any{4} >{ shares   = __builtin_bswap32(*(const uint32_t*)(p)); };
    orderExecutedMatchNum8 = any{8} >{ matchNum = __builtin_bswap64(*(const uint64_t*)(p)); };
    orderExecutedRefNum8   = any{8} >{ refNum   = __builtin_bswap64(*(const uint64_t*)(p)); };

    orderExecutedCommon = orderExecutedRefNum8
                          orderExecutedShares4
                          orderExecutedMatchNum8;

    # 4.5.1 Order Executed Message
    orderExecuted = 'E' %{ type = 1; }
                    nanoseconds
                    orderExecutedCommon;

    # 4.5.2 Order Executed With Price Message
    orderExecutedPrintable1 = 'Y' %{ printable = true;  }
                            | 'N' %{ printable = false; };

    orderExecutedPrice4 = any{4} >{ price = __builtin_bswap32(*(const uint32_t*)(p)); };

    orderExecutedWithPrice = 'C' %{ type = 2; }
                             nanoseconds
                             orderExecutedCommon
                             orderExecutedPrintable1
                             orderExecutedPrice4;
}%%

// define a function pointer type that matches the STG calling convention
typedef void (*HsCall)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t,
                       int64_t, int64_t, int64_t*, float, float, float, float, double, double);

extern void
ITCHv41_run(
    int64_t* restrict baseReg,
    int64_t* restrict sp,
    int64_t* restrict hp,
    const uint8_t* restrict buffer, // R1
    int64_t length, // R2
    int64_t r3,
    int64_t r4,
    int64_t r5,
    int64_t r6,
    int64_t* restrict spLim,
    float f1,
    float f2,
    float f3,
    float f4,
    double d1,
    double d2)
{
    %% main := orderExecuted | orderExecutedWithPrice;

    int cs;
    %% write init;

    const uint8_t* p = buffer;
    const uint8_t* pe = &buffer[length];

    // create undefined variables, clang will emit these as a llvm undef literal
    const int64_t iUndef;
    const float fUndef;
    const double dUndef;

    uint32_t type;
    uint32_t nanos;
    uint32_t shares;
    uint32_t price;
    uint64_t matchNum;
    uint64_t refNum;
    bool printable;

    %% write exec;

    const HsCall fun = (HsCall)sp[0];

    // the machine has completed
    if (cs == ITCHv41_first_final)
    {
        return fun(
            baseReg,
            sp, // updated_stack,
            hp,
            type,
            nanos,
            shares,
            price,
            matchNum,
            refNum,
            spLim,
            fUndef,
            fUndef,
            fUndef,
            fUndef,
            dUndef,
            dUndef);
    }
    else if (cs == ITCHv41_error)
    {
        return fun(
            baseReg,
            sp, // updated_stack,
            hp,
            -1,
            iUndef,
            iUndef,
            iUndef,
            iUndef,
            iUndef,
            spLim,
            fUndef,
            fUndef,
            fUndef,
            fUndef,
            dUndef,
            dUndef);
    }
    else
    {
        return fun(
            baseReg,
            sp, // updated_stack,
            hp,
            -2,
            iUndef,
            iUndef,
            iUndef,
            iUndef,
            iUndef,
            spLim,
            fUndef,
            fUndef,
            fUndef,
            fUndef,
            dUndef,
            dUndef);
    }
}
