//
//  main.c
//  mos6502
//
//  Created by yuta on 2020/07/29.
//  Copyright © 2020 yuta. All rights reserved.
//

#include <stdio.h>

#define true 1
#define false 0

#define OPERATION_TABLE_LENGTH 256
#define ZERO_PAGE_MAX 256
#define LINE_SIZE 40
#define CHAR_NUM 256
#define MEMORY_SIZE 0xfffff

#define IRQ_VECTOR 0xfffe
#define RESET_VECTOR 0xfffc

#define KEY_I_ADDRESS 0xc000
#define KEY_CLEAR_ADDRESS 0xc010

#define RAM_SIZE 0xbfff
#define SYSTEM_ROM_START_ADDRESS 0xd000
#define SYSTEM_ROM_SIZE 12288

#define MEMORY_FILE_NAME "SYSTEM_DISK.bin"
#define ROM_FILE_NAME "ROM.bin"


enum {
    accumulator,
    immediate,
    zeroPage,
    zeroPageX,
    zeroPageY,
    absolute,
    absoluteX,
    absoluteY,
    indirect,
    indirectX,
    indirectY,
    relative,
    implied
};

enum{
    adc,
    and,
    asl,
    bcc,
    bcs,
    beq,
    bit,
    bmi,
    bne,
    bpl,
    brk,
    bvc,
    bvs,
    clc,
    cld,
    cli,
    clv,
    cmp,
    cpx,
    cpy,
    dec,
    dex,
    dey,
    eor,
    inc,
    inx,
    iny,
    jmp,
    jsr,
    lda,
    ldx,
    ldy,
    lsr,
    nop,
    ora,
    pha,
    php,
    pla,
    plp,
    rol,
    ror,
    rti,
    rts,
    sbc,
    sec,
    sed,
    sei,
    sta,
    stx,
    sty,
    tax,
    tay,
    tsx,
    txa,
    txs,
    tya
};

typedef struct{
    char opcode;
    int opcodeName;
    int addressingMode;
}t_operationTable;

t_operationTable operationTable[OPERATION_TABLE_LENGTH] = {
    /* opcode   opcodeName  addressingMode**/
    { 0x69, adc, immediate },
    { 0x65, adc, zeroPage },
    { 0x75, adc, zeroPageX },
    { 0x6d, adc, absolute },
    { 0x7d, adc, absoluteX },
    { 0x79, adc, absoluteY },
    { 0x61, adc, indirectX },
    { 0x71, adc, indirectY },
    { 0x29, and, immediate },
    { 0x25, and, zeroPage },
    { 0x35, and, zeroPageX },
    { 0x2d, and, absolute },
    { 0x3d, and, absoluteX },
    { 0x39, and, absoluteY },
    { 0x21, and, indirectX },
    { 0x31, and, indirectY },
    { 0x0a, asl, accumulator },
    { 0x06, asl, zeroPage },
    { 0x16, asl, zeroPageX },
    { 0x0e, asl, absolute },
    { 0x1e, asl, absoluteX },
    { 0x90, bcc, relative },
    { 0xb0, bcs, relative },
    { 0xf0, beq, relative },
    { 0x24, bit, zeroPage },
    { 0x2c, bit, absolute },
    { 0x30, bmi, relative },
    { 0xd0, bne, relative },
    { 0x10, bpl, relative },
    { 0x00, brk, implied },
    { 0x50, bvc, relative },
    { 0x70, bvs, relative },
    { 0x18, clc, implied },
    { 0xd8, cld, implied },
    { 0x58, cli, implied },
    { 0xb8, clv, implied },
    { 0xc9, cmp, immediate },
    { 0xc5, cmp, zeroPage },
    { 0xd5, cmp, zeroPageX },
    { 0xcd, cmp, absolute },
    { 0xdd, cmp, absoluteX },
    { 0xd9, cmp, absoluteY },
    { 0xc1, cmp, indirectX },
    { 0xd1, cmp, indirectY },
    { 0xe0, cpx, immediate },
    { 0xe4, cpx, zeroPage },
    { 0xdc, cpx, absolute },
    { 0xc0, cpy, immediate },
    { 0xc4, cpy, zeroPage },
    { 0xcc, cpy, absolute },
    { 0xc6, dec, zeroPage },
    { 0xd6, dec, zeroPageX },
    { 0xce, dec, absolute },
    { 0xde, dec, absoluteX },
    { 0xca, dex, implied },
    { 0x88, dey, implied },
    { 0x49, eor, immediate },
    { 0x45, eor, zeroPage },
    { 0x55, eor, zeroPageX },
    { 0x4d, eor, absolute },
    { 0x5d, eor, absoluteX },
    { 0x59, eor, absoluteY },
    { 0x41, eor, indirectX },
    { 0x51, eor, indirectY },
    { 0xe6, inc, zeroPage },
    { 0xf6, inc, zeroPageX },
    { 0xee, inc, absolute },
    { 0xfe, inc, absoluteX },
    { 0xe8, inx, implied },
    { 0xc8, iny, implied },
    { 0x4c, jmp, absolute },
    { 0x6c, jmp, indirect },
    { 0x20, jsr, absolute },
    { 0xa9, lda, immediate },
    { 0xa5, lda, zeroPage },
    { 0xb5, lda, zeroPageX },
    { 0xad, lda, absolute },
    { 0xbd, lda, absoluteX },
    { 0xb9, lda, absoluteY },
    { 0xa1, lda, indirectX },
    { 0xb1, lda, indirectY },
    { 0xa2, ldx, immediate },
    { 0xa6, ldx, zeroPage },
    { 0xb6, ldx, zeroPageY},
    { 0xae, ldx, absolute },
    { 0xbe, ldx, absoluteY },
    { 0xa0, ldy, immediate },
    { 0xa4, ldy, zeroPage },
    { 0xb4, ldy, zeroPageX },
    { 0xac, ldy, absolute },
    { 0xbc, ldy, absoluteX },
    { 0x4a, lsr, accumulator },
    { 0x46, lsr, zeroPage },
    { 0x56, lsr, zeroPageX },
    { 0x4e, lsr, absolute },
    { 0x5e, lsr, absoluteX },
    { 0xea, nop, implied },
    { 0x09, ora, immediate },
    { 0x05, ora, zeroPage },
    { 0x15, ora, zeroPageX },
    { 0x0d, ora, absolute },
    { 0x1d, ora, absoluteX },
    { 0x19, ora, absoluteY },
    { 0x01, ora, indirectX },
    { 0x11, ora, indirectY },
    { 0x48, pha, implied },
    { 0x08, php, implied },
    { 0x68, pla, implied },
    { 0x28, plp, implied },
    { 0x2a, rol, accumulator },
    { 0x26, rol, zeroPage },
    { 0x36, rol, zeroPageX },
    { 0x2e, rol, absolute },
    { 0x3e, rol, absoluteX },
    { 0x6a, ror, accumulator },
    { 0x66, ror, zeroPage },
    { 0x76, ror, zeroPageX },
    { 0x6e, ror, absolute },
    { 0x7e, ror, absoluteX},
    { 0x40, rti, implied },
    { 0x60, rts, implied },
    { 0xe9, sbc, immediate },
    { 0xe5, sbc, zeroPage },
    { 0xf5, sbc, zeroPageX },
    { 0xed, sbc, absolute },
    { 0xfd, sbc, absoluteX },
    { 0xf9, sbc, absoluteY },
    { 0xe1, sbc, indirectX },
    { 0xf1, sbc, indirectY },
    { 0x38, sec, implied },
    { 0xf8, sed, implied },
    { 0x78, sei, implied },
    { 0x85, sta, zeroPage },
    { 0x95, sta, zeroPageX },
    { 0x8d, sta, absolute },
    { 0x9d, sta, absoluteX },
    { 0x99, sta, absoluteY },
    { 0x81, sta, indirectX },
    { 0x91, sta, indirectY },
    { 0x86, stx, zeroPage },
    { 0x96, stx, zeroPageY },
    { 0x8e, stx, absolute },
    { 0x84, sty, zeroPage },
    { 0x94, sty, zeroPageX },
    { 0x8c, sty, absolute },
    { 0xaa, tax, implied },
    { 0xa8, tay, implied },
    { 0xba, tsx, implied },
    { 0x8a, txa, implied },
    { 0x9a, txs, implied },
    { 0x98, tya, implied }
};


typedef struct{
    int code;
    char printChar;
}t_convertTbl;


t_convertTbl convertTbl[CHAR_NUM] = {
    {0x00, '@'},
    {0x01, 'A'},
    {0x02, 'B'},
    {0x03, 'C'},
    {0x04, 'D'},
    {0x05, 'E'},
    {0x06, 'F'},
    {0x07, 'G'},
    {0x08, 'H'},
    {0x09, 'I'},
    {0x0a, 'J'},
    {0x0b, 'K'},
    {0x0c, 'L'},
    {0x0d, 'M'},
    {0x0e, 'N'},
    {0x0f, 'O'},
    {0x10, 'P'},
    {0x11, 'Q'},
    {0x12, 'R'},
    {0x13, 'S'},
    {0x14, 'T'},
    {0x15, 'U'},
    {0x16, 'V'},
    {0x17, 'W'},
    {0x18, 'X'},
    {0x19, 'Y'},
    {0x1a, 'Z'},
    {0x1b, '['},
    {0x1c, '\\'},
    {0x1d, ']'},
    {0x1e, '^'},
    {0x1f, '_'},
    {0x20, ' '},
    {0x21, '!'},
    {0x22, '"'},
    {0x23, '#'},
    {0x24, '$'},
    {0x25, '%'},
    {0x26, '&'},
    {0x27, '\''},
    {0x28, '('},
    {0x29, ')'},
    {0x2a, '*'},
    {0x2b, '+'},
    {0x2c, ','},
    {0x2d, '-'},
    {0x2e, '.'},
    {0x2f, '/'},
    {0x30, '0'},
    {0x31, '1'},
    {0x32, '2'},
    {0x33, '3'},
    {0x34, '4'},
    {0x35, '5'},
    {0x36, '6'},
    {0x37, '7'},
    {0x38, '8'},
    {0x39, '9'},
    {0x3a, ':'},
    {0x3b, ';'},
    {0x3c, '<'},
    {0x3d, '='},
    {0x3e, '>'},
    {0x3f, '?'},
    {0x40, '@'},
    {0x41, 'A'},
    {0x42, 'B'},
    {0x43, 'C'},
    {0x44, 'D'},
    {0x45, 'E'},
    {0x46, 'F'},
    {0x47, 'G'},
    {0x48, 'H'},
    {0x49, 'I'},
    {0x4a, 'J'},
    {0x4b, 'K'},
    {0x4c, 'L'},
    {0x4d, 'M'},
    {0x4e, 'N'},
    {0x4f, 'O'},
    {0x50, 'P'},
    {0x51, 'Q'},
    {0x52, 'R'},
    {0x53, 'S'},
    {0x54, 'T'},
    {0x55, 'U'},
    {0x56, 'V'},
    {0x57, 'W'},
    {0x58, 'X'},
    {0x59, 'Y'},
    {0x5a, 'Z'},
    {0x5b, '['},
    {0x5c, '\\'},
    {0x5d, ']'},
    {0x5e, '^'},
    {0x5f, '_'},
    {0x60, ' '},
    {0x61, '!'},
    {0x62, '"'},
    {0x63, '#'},
    {0x64, '$'},
    {0x65, '%'},
    {0x66, '&'},
    {0x67, '\''},
    {0x68, '('},
    {0x69, ')'},
    {0x6a, '*'},
    {0x6b, '+'},
    {0x6c, ','},
    {0x6d, '-'},
    {0x6e, '.'},
    {0x6f, '/'},
    {0x70, '0'},
    {0x71, '1'},
    {0x72, '2'},
    {0x73, '3'},
    {0x74, '4'},
    {0x75, '5'},
    {0x76, '6'},
    {0x77, '7'},
    {0x78, '8'},
    {0x79, '9'},
    {0x7a, ':'},
    {0x7b, ';'},
    {0x7c, '<'},
    {0x7d, '='},
    {0x7e, '>'},
    {0x7f, '?'},
    {0x80, '@'},
    {0x81, 'A'},
    {0x82, 'B'},
    {0x83, 'C'},
    {0x84, 'D'},
    {0x85, 'E'},
    {0x86, 'F'},
    {0x87, 'G'},
    {0x88, 'H'},
    {0x89, 'I'},
    {0x8a, 'J'},
    {0x8b, 'K'},
    {0x8c, 'L'},
    {0x8d, 'M'},
    {0x8e, 'N'},
    {0x8f, 'O'},
    {0x90, 'P'},
    {0x91, 'Q'},
    {0x92, 'R'},
    {0x93, 'S'},
    {0x94, 'T'},
    {0x95, 'U'},
    {0x96, 'V'},
    {0x97, 'W'},
    {0x98, 'X'},
    {0x99, 'Y'},
    {0x9a, 'Z'},
    {0x9b, '['},
    {0x9c, '\\'},
    {0x9d, ']'},
    {0x9e, '^'},
    {0x9f, '_'},
    {0xa0, ' '},
    {0xa1, '!'},
    {0xa2, '"'},
    {0xa3, '#'},
    {0xa4, '$'},
    {0xa5, '%'},
    {0xa6, '&'},
    {0xa7, '\''},
    {0xa8, '('},
    {0xa9, ')'},
    {0xaa, '*'},
    {0xab, '+'},
    {0xac, ','},
    {0xad, '-'},
    {0xae, '.'},
    {0xaf, '/'},
    {0xb0, '0'},
    {0xb1, '1'},
    {0xb2, '2'},
    {0xb3, '3'},
    {0xb4, '4'},
    {0xb5, '5'},
    {0xb6, '6'},
    {0xb7, '7'},
    {0xb8, '8'},
    {0xb9, '9'},
    {0xba, ':'},
    {0xbb, ';'},
    {0xbc, '<'},
    {0xbd, '='},
    {0xbe, '>'},
    {0xbf, '?'},
    {0xc0, '@'},
    {0xc1, 'A'},
    {0xc2, 'B'},
    {0xc3, 'C'},
    {0xc4, 'D'},
    {0xc5, 'E'},
    {0xc6, 'F'},
    {0xc7, 'G'},
    {0xc8, 'H'},
    {0xc9, 'I'},
    {0xca, 'J'},
    {0xcb, 'K'},
    {0xcc, 'L'},
    {0xcd, 'M'},
    {0xce, 'N'},
    {0xcf, 'O'},
    {0xd0, 'P'},
    {0xd1, 'Q'},
    {0xd2, 'R'},
    {0xd3, 'S'},
    {0xd4, 'T'},
    {0xd5, 'U'},
    {0xd6, 'V'},
    {0xd7, 'W'},
    {0xd8, 'X'},
    {0xd9, 'Y'},
    {0xda, 'Z'},
    {0xdb, '['},
    {0xdc, '\\'},
    {0xdd, ']'},
    {0xde, '^'},
    {0xdf, '_'},
    {0xe0, ' '},
    {0xe1, '!'},
    {0xe2, '"'},
    {0xe3, '#'},
    {0xe4, '$'},
    {0xe5, '%'},
    {0xe6, '&'},
    {0xe7, '\''},
    {0xe8, '('},
    {0xe9, ')'},
    {0xea, '*'},
    {0xeb, '+'},
    {0xec, ','},
    {0xed, '-'},
    {0xee, '.'},
    {0xef, '/'},
    {0xf0, '0'},
    {0xf1, '1'},
    {0xf2, '2'},
    {0xf3, '3'},
    {0xf4, '4'},
    {0xf5, '5'},
    {0xf6, '6'},
    {0xf7, '7'},
    {0xf8, '8'},
    {0xf9, '9'},
    {0xfa, ':'},
    {0xfb, ';'},
    {0xfc, '<'},
    {0xfd, '='},
    {0xfe, '>'},
    {0xff, '?'}
};


typedef struct{
    int addressingMode;
    int opcode;
}t_decoded;


typedef struct{
    int opcode;
    int useRegister;
    char *operand;
    char *operand2;
    int useOperandFlg;
}t_inst;


typedef struct{
    int programCounter;
    char accumulator;
    char indexX;
    char indexY;
    char stackPointer;
} t_regist;


typedef struct{
   unsigned int negative : 1;
   unsigned int overFlow : 1;
   unsigned int dummy : 1;
   unsigned int breakCommand : 1;
   unsigned int decimalMode : 1;
   unsigned int interaputDisable : 1;
   unsigned int zero : 1;
   unsigned int carry : 1;
}t_statusBits;


typedef union{
    char buffer;
    t_statusBits bits;
} t_statusRegist;


char getOpecode( int address, char *memory ){
    char opcode = memory[address];
    return opcode;
}


void addressSelector( char *memory ){
    
}


t_decoded decode( char opcode ){
    t_decoded decoded = { 0 };
    
    for(int i = 0; OPERATION_TABLE_LENGTH > i; i++){
        if(operationTable[i].opcode == opcode){
            decoded.opcode = operationTable[i].opcodeName;
            decoded.addressingMode = operationTable[i].addressingMode;
            break;
        }
    }
    
    return decoded;
}

void getInst( t_regist *regist, char *memory, t_inst *inst ){
    //t_inst inst = { 0 };
    int address = 0;
    int preAddress = 0;
    int preAddress2 = 0;
    int operand = 0;
    
    // get opcode
    char opcode = getOpecode( regist->programCounter, memory );
    
    //test
    //int test = opcode & 0xff;
    //printf("opcode :%x\n\n", test);
    
    // addressing
    t_decoded decoded = decode( opcode );
    inst->opcode = decoded.opcode;
    
    switch( decoded.addressingMode ){
        case accumulator:
            inst->operand = &regist->accumulator;
            inst->useOperandFlg = true;
            regist->programCounter += 1;
            break;
            
        case immediate:
            inst->operand = &memory[regist->programCounter + 1];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case zeroPage:
            address = memory[regist->programCounter + 1] & 0xff;
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case zeroPageX:
            preAddress = memory[regist->programCounter + 1] + regist->indexX;
            address = preAddress & 0xff;
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case zeroPageY:
            preAddress = memory[regist->programCounter + 1] + regist->indexY;
            address = preAddress & 0xff;
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case absolute:
            if( (inst->opcode == jsr) || (inst->opcode == jmp) ){
                inst->operand = &memory[regist->programCounter + 1];
                inst->operand2 = &memory[regist->programCounter + 2];
                //inst->intOperand = &address;
                //testInst->intOperand = &address;
                inst->useOperandFlg = false;
            }
            else{
                address = memory[regist->programCounter + 1] & 0xff;
                address |= ((memory[regist->programCounter + 2] << 8) & 0xff00);
                inst->operand = &memory[address];
                inst->useOperandFlg = true;
                regist->programCounter += 3;
            }
            break;
            
        case absoluteX:
            address = memory[regist->programCounter + 1] & 0xff;
            address |= ((memory[regist->programCounter + 2] << 8) & 0xff00);
            address += regist->indexX;
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 3;
            break;
            
        case absoluteY:
            address = memory[regist->programCounter + 1] & 0xff;
            address |= ((memory[regist->programCounter + 2] << 8) & 0xff00);
            address += regist->indexY;
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 3;
            break;
            
        case indirect:
            preAddress = memory[regist->programCounter + 1] & 0xff;
            preAddress |= ((memory[regist->programCounter + 2] << 8) & 0xff00);
            inst->operand = &memory[preAddress];
            inst->operand2 = &memory[preAddress + 1];
            //inst->operand = &memory[address];
            inst->useOperandFlg = false;
            regist->programCounter += 3;
            break;
            
        case indirectX:
            operand = memory[regist->programCounter + 1];
            preAddress = memory[operand] & 0xff;
            preAddress = preAddress + regist->indexX;
            if( preAddress > ZERO_PAGE_MAX ){
                preAddress = preAddress & 0xff;
            }
            
            if( preAddress + 1 > ZERO_PAGE_MAX ){
                preAddress2 = (preAddress + 1) & 0xff;
            }
            else{
                preAddress2 = preAddress + 1;
            }
            address = memory[preAddress] & 0xff;
            address |= ((memory[preAddress2] << 8) & 0xff00);
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case indirectY:
            operand = memory[regist->programCounter + 1] & 0xff;
            preAddress = memory[operand] & 0xff;
            if(operand + 1 > ZERO_PAGE_MAX ){
                preAddress2 = (operand + 1) & 0xff;
            }
            else{
                preAddress2 = operand + 1;
            }
            preAddress |= ((memory[preAddress2] << 8) & 0xff00);
            address = preAddress + regist->indexY;
            inst->operand = &memory[address];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case relative:
            inst->operand = &memory[regist->programCounter + 1];
            inst->useOperandFlg = true;
            regist->programCounter += 2;
            break;
            
        case implied:
            break;
            
        default:
            break;
    }
    
    //return inst;
}

void updateCarry( t_statusRegist *statusRegist, int tmp){
    if(((tmp & 0x100) >> 8) == 1){
        statusRegist->bits.carry = true;
    }
    else{
        statusRegist->bits.carry = false;
    }
    
}

void updateOverFlow( t_statusRegist *statusRegist,int operand, int accum, int tmp){
    
    if( (((operand & 0x80) >> 7) == 0) && (((accum & 0x80) >> 7) == 0)
       && ( ((tmp & 0x80) >> 7) == 1 ) && ( ((tmp & 0x100) >> 8) == 0)  ){
        statusRegist->bits.overFlow = true;
    }
    else{
        statusRegist->bits.overFlow = false;
    }
}

void updateNegative( t_statusRegist *statusRegist, int tmp){
    
    if(((tmp & 0x80) >> 7) == 1){
        statusRegist->bits.negative = true;
    }
    else{
        statusRegist->bits.negative = false;
    }
    
}

void updateZero( t_statusRegist *statusRegist, int tmp){
    
    if( tmp == 0 ){
        statusRegist->bits.zero = true;
    }
    else{
        statusRegist->bits.zero = false;
    }
}

void pushToStack( int programCounter, t_statusRegist statusRegist, char *stackPointer, char *memory){
    int stackAddress = ((*stackPointer) | 0x100) & 0x1ff;
    
    memory[stackAddress--] = (programCounter & 0xff00) >> 8;
    memory[stackAddress--] = programCounter & 0xff;
    memory[stackAddress--] = statusRegist.buffer;
    
    *stackPointer = stackAddress & 0xff;
}


void pushToStack2( int programCounter, char *stackPointer, char *memory){
    int stackAddress = ((*stackPointer) | 0x100) & 0x1ff;
 
    memory[stackAddress--] = (programCounter & 0xff00) >> 8;
    memory[stackAddress--] = programCounter & 0xff;
    
    *stackPointer = stackAddress & 0xff;
}

void pushToStack3( char data, char *stackPointer, char *memory){
    int stackAddress = ((*stackPointer) | 0x100) & 0x1ff;
    
    memory[stackAddress--] = data;
    
    *stackPointer = stackAddress & 0xff;
}




int pullFromStack2(char *stackPointer, char *memory){
    int data;
    int stackAddress = ((*stackPointer) | 0x100) & 0x1ff;

    data = memory[++stackAddress] & 0xff;
    data |= ((memory[++stackAddress] << 8 ) & 0xff00);
    *stackPointer = stackAddress & 0xff;
    
    return data;
}


char pullFromStack3(char *stackPointer, char *memory){
    char data;
    int stackAddress = ((*stackPointer) | 0x100) & 0x1ff;
    
    data = memory[++stackAddress];
    *stackPointer = stackAddress & 0xff;
    
    return data;
}

void execute(t_inst inst, t_regist *regist, t_statusRegist *statusRegist, char *memory){
    int tmp = 0;
    int decimalAcumLow = 0;
    int decimalAcumHigh = 0;
    int decimalAcum = 0;
    int decimalOpLow = 0;
    int decimalOpHigh = 0;
    int decimalOp = 0;
    
    switch( inst.opcode ){
        case adc:
            if(statusRegist->bits.decimalMode == false){
                tmp = regist->accumulator + *inst.operand + statusRegist->bits.carry;
                updateCarry(statusRegist, tmp);
                updateOverFlow(statusRegist, *inst.operand, regist->accumulator, tmp);
                updateNegative(statusRegist, tmp);
                updateZero(statusRegist, tmp);
                //*inst.operand = tmp;
                regist->accumulator = tmp;
            }
            else{
                decimalAcumLow = regist->accumulator & 0xff;
                decimalAcumHigh = (regist->accumulator & 0xff00) >> 8;
                decimalAcum = (decimalAcumHigh * 10) + decimalAcumLow;
                
                decimalOpLow = *inst.operand & 0xff;
                decimalOpHigh = (*inst.operand & 0xff00) >> 8;
                decimalOp = (decimalOpHigh * 10) + decimalOpLow;
                
                tmp = decimalAcum + decimalOp + statusRegist->bits.carry;

                updateCarry(statusRegist, tmp);
                updateOverFlow(statusRegist, *inst.operand, regist->accumulator, tmp);
                updateNegative(statusRegist, tmp);
                updateZero(statusRegist, tmp);
                //*inst.operand = tmp;
                regist->accumulator = tmp;
            }
            
            break;
            
        case and:
            tmp = regist->accumulator & *inst.operand;
            updateNegative(statusRegist, tmp);
            updateZero(statusRegist, tmp);
            //*inst.operand = tmp;
            regist->accumulator = tmp;
            break;
            
        case asl:
            statusRegist->bits.carry = (*inst.operand & 0x80) >> 7;
            *inst.operand = (*inst.operand << 1) & 0xff;
            updateZero(statusRegist, *inst.operand);
            updateNegative(statusRegist, *inst.operand);
            break;
            
        case bcc:
            if(statusRegist->bits.carry == false){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
                
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case bcs:
            if(statusRegist->bits.carry == true){
                if( *inst.operand & 0x80 ){
                    tmp = *inst.operand & 0x7e;
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
                
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case beq:
            if(statusRegist->bits.zero == true){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
                
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case bit:
            tmp = *inst.operand & regist->accumulator;
            updateZero(statusRegist, tmp);
            statusRegist->bits.negative = (tmp & 0x80) >> 7;
            statusRegist->bits.overFlow = (tmp & 0x40) >> 6;
            break;
            
        case bmi:
            if(statusRegist->bits.negative == true){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
                    
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case bne:
            if(statusRegist->bits.zero == false){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case bpl:
            if(statusRegist->bits.negative == false){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
                    
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case brk:
            statusRegist->bits.breakCommand = true;
            pushToStack(regist->programCounter + 2, *statusRegist,
                        &regist->stackPointer, memory);
            regist->programCounter = memory[IRQ_VECTOR] & 0xff;
            regist->programCounter |= ((memory[IRQ_VECTOR + 1] << 8) & 0xff00);
            break;
            
        case bvc:
            if(statusRegist->bits.overFlow == false){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case bvs:
            if(statusRegist->bits.overFlow == true){
                if( *inst.operand & 0x80 ){
                    regist->programCounter += ((*inst.operand) | 0xff00);
                    regist->programCounter &= 0xffff;
                }
                else{
                    regist->programCounter += (*inst.operand) & 0xff;
                }
            }
            else{
                //regist->programCounter += 2;
            }
            break;
            
        case clc:
            statusRegist->bits.carry = false;
            regist->programCounter += 1;
            break;
            
        case cld:
            statusRegist->bits.decimalMode = false;
            regist->programCounter += 1;
            break;
            
        case cli:
            statusRegist->bits.interaputDisable = false;
            regist->programCounter += 1;
            break;
            
        case clv:
            statusRegist->bits.overFlow = false;
            regist->programCounter += 1;
            break;
            
        case cmp:
            tmp = regist->accumulator - *inst.operand;
            updateZero(statusRegist, tmp);
            updateNegative(statusRegist, tmp);
            statusRegist->bits.carry = !statusRegist->bits.negative;
            break;
            
        case cpx:
            tmp = regist->indexX - *inst.operand;
            updateZero(statusRegist, tmp);
            updateNegative(statusRegist, tmp);
            statusRegist->bits.carry = !statusRegist->bits.negative;
            break;
            
        case cpy:
            tmp = regist->indexY - *inst.operand;
            updateZero(statusRegist, tmp);
            updateNegative(statusRegist, tmp);
            statusRegist->bits.carry = !statusRegist->bits.negative;
            break;
            
        case dec:
            *inst.operand -= 1;
            updateZero(statusRegist, *inst.operand);
            updateNegative(statusRegist, *inst.operand);
            regist->programCounter += 1;
            break;
            
        case dex:
            regist->indexX -= 1;
            updateZero(statusRegist, regist->indexX);
            updateNegative(statusRegist, regist->indexX);
            regist->programCounter += 1;
            break;
            
        case dey:
            regist->indexY -= 1;
            updateZero(statusRegist, regist->indexY);
            updateNegative(statusRegist, regist->indexY);
            regist->programCounter += 1;
            break;
            
        case eor:
            regist->accumulator = *inst.operand ^ regist->accumulator;
            updateZero(statusRegist, regist->accumulator);
            updateNegative(statusRegist, regist->accumulator);
            break;
            
        case inc:
            *inst.operand += 1;
            updateZero(statusRegist, *inst.operand);
            updateNegative(statusRegist, *inst.operand);
            break;
            
        case inx:
            regist->indexX += 1;
            updateZero(statusRegist, regist->indexX);
            updateNegative(statusRegist, regist->indexX);
            regist->programCounter += 1;
            break;
            
        case iny:
            regist->indexY += 1;
            updateZero(statusRegist, regist->indexY);
            updateNegative(statusRegist, regist->indexY);
            regist->programCounter += 1;
            break;
            
        case jmp:
            if( inst.useOperandFlg ){
                regist->programCounter = *inst.operand & 0xff;
            }
            else{
                regist->programCounter = *inst.operand & 0xff;
                regist->programCounter |= ((*inst.operand2 << 8) & 0xff00);
                //regist->programCounter = *inst.intOperand & 0xffff;
            }
            break;
            
        case jsr:
            pushToStack2(regist->programCounter + 2, &regist->stackPointer, memory);
            //regist->programCounter = *inst.intOperand & 0xffff;
            //regist->programCounter = *testInst.intOperand & 0xffff;
            regist->programCounter = *inst.operand & 0xff;
            regist->programCounter |= ((*inst.operand2 << 8) & 0xff00);
            break;
            
        case lda:
            regist->accumulator = *inst.operand;
            updateZero(statusRegist, regist->accumulator);
            updateNegative(statusRegist, regist->accumulator);
            break;
            
        case ldx:
            regist->indexX = *inst.operand;
            updateZero(statusRegist, regist->indexX);
            updateNegative(statusRegist, regist->indexX);
            break;
            
        case ldy:
            regist->indexY = *inst.operand;
            updateZero(statusRegist, regist->indexY);
            updateNegative(statusRegist, regist->indexY);
            break;
            
        case lsr:
            statusRegist->bits.carry = *inst.operand & 0x01;
            *inst.operand >>= 1;
            statusRegist->bits.negative = false;
            updateZero(statusRegist, *inst.operand);
            break;
            
        case nop:
            regist->programCounter += 1;
            break;
            
        case ora:
            regist->accumulator |= *inst.operand;
            updateZero(statusRegist, regist->accumulator);
            updateNegative(statusRegist, regist->accumulator);
            break;
            
        case pha:
            pushToStack3(regist->accumulator, &regist->stackPointer, memory);
            regist->programCounter += 1;
            break;
            
        case php:
            pushToStack3(statusRegist->buffer, &regist->stackPointer, memory);
            regist->programCounter += 1;
            break;
            
        case pla:
            regist->accumulator = pullFromStack3(&regist->stackPointer, memory);
            regist->programCounter += 1;
            break;
            
        case plp:
            statusRegist->buffer = pullFromStack3(&regist->stackPointer, memory);
            regist->programCounter += 1;
            break;
            
        case rol:
            tmp = statusRegist->bits.carry & 0x01;
            statusRegist->bits.carry = (*inst.operand & 0x80) >> 8;
            *inst.operand <<= 1;
            *inst.operand |= tmp;
            updateZero(statusRegist, *inst.operand);
            updateNegative(statusRegist, *inst.operand);
            break;
            
        case ror:
            tmp = *inst.operand & 0x01;
            *inst.operand >>= 1;
            *inst.operand |= (statusRegist->bits.carry << 7);
            statusRegist->bits.carry = tmp;
            updateZero(statusRegist, *inst.operand);
            updateNegative(statusRegist, *inst.operand);
            break;
            
        case rti:
            statusRegist->buffer = pullFromStack3(&regist->stackPointer, memory);
            regist->programCounter = pullFromStack2(&regist->stackPointer, memory);
            break;
            
        case rts:
            regist->programCounter = pullFromStack2(&regist->stackPointer, memory);
            regist->programCounter += 1;
            break;
            
        case sbc:
            if(statusRegist->bits.decimalMode == false){
                tmp = regist->accumulator - *inst.operand - (!statusRegist->bits.carry);
                updateZero(statusRegist, tmp);
                updateNegative(statusRegist, tmp);
                updateCarry(statusRegist, tmp);
                updateOverFlow(statusRegist, *inst.operand, regist->accumulator, tmp);
                //*inst.operand = tmp;
                regist->accumulator = tmp;
            }
            else{
                decimalAcumLow = regist->accumulator & 0xff;
                decimalAcumHigh = (regist->accumulator & 0xff00) >> 8;
                decimalAcum = (decimalAcumHigh * 10) + decimalAcumLow;
                
                decimalOpLow = *inst.operand & 0xff;
                decimalOpHigh = (*inst.operand & 0xff00) >> 8;
                decimalOp = (decimalOpHigh * 10) + decimalOpLow;

                tmp = regist->accumulator - *inst.operand - (!statusRegist->bits.carry);
                updateZero(statusRegist, tmp);
                updateNegative(statusRegist, tmp);
                updateCarry(statusRegist, tmp);
                updateOverFlow(statusRegist, *inst.operand, regist->accumulator, tmp);
                //*inst.operand = tmp;
                regist->accumulator = tmp;
            }
            break;
            
        case sec:
            statusRegist->bits.carry = true;
            regist->programCounter += 1;
            break;
            
        case sed:
            statusRegist->bits.decimalMode = true;
            regist->programCounter += 1;
            break;
            
        case sei:
            statusRegist->bits.interaputDisable = true;
            regist->programCounter += 1;
            break;
            
        case sta:
            *inst.operand = regist->accumulator;
            break;
            
        case stx:
            *inst.operand = regist->indexX;
            break;
            
        case sty:
            *inst.operand = regist->indexY;
            break;
            
        case tax:
            regist->indexX = regist->accumulator;
            updateZero(statusRegist, regist->indexX);
            updateNegative(statusRegist, regist->indexX);
            regist->programCounter += 1;
            break;
            
        case tay:
            regist->indexY = regist->accumulator;
            updateZero(statusRegist, regist->indexY);
            updateNegative(statusRegist, regist->indexY);
            regist->programCounter += 1;
            break;
            
        case tsx:
            regist->indexX = regist->stackPointer;
            updateZero(statusRegist, regist->indexX);
            updateNegative(statusRegist, regist->indexX);
            regist->programCounter += 1;
            break;
            
        case txa:
            regist->accumulator = regist->indexX;
            updateZero(statusRegist, regist->accumulator);
            updateNegative(statusRegist, regist->accumulator);
            regist->programCounter += 1;
            break;
            
        case txs:
            regist->stackPointer = regist->indexX;
            regist->programCounter += 1;
            break;
            
        case tya:
            regist->accumulator = regist->indexY;
            updateZero(statusRegist, regist->accumulator);
            updateNegative(statusRegist, regist->accumulator);
            regist->programCounter += 1;
            break;
            
        default:
            break;
    }
}


void getKeyInput( char *memory ){
    char inputBuf = '\0';
    
    scanf("%c", &inputBuf);
    
    if(inputBuf != '\0'){
        inputBuf |= 0x80;
    }
    
    memory[KEY_I_ADDRESS] = inputBuf;
}


void initMemory( char *memory ){
    FILE *fp;
    
    //fp = fopen(MEMORY_FILE_NAME, "rb");
    //fread(memory, sizeof(char), RAM_SIZE, fp);
    //fclose(fp);
    
    fp =fopen(ROM_FILE_NAME, "rb");
    fread(&memory[SYSTEM_ROM_START_ADDRESS], sizeof(char), SYSTEM_ROM_SIZE, fp);
    fclose(fp);
    
}


void initRegist( t_regist *regist, char *memory ){
    
    regist->stackPointer = 0xff;
    int entryPoint = memory[RESET_VECTOR] & 0xff;
    entryPoint |= ((memory[RESET_VECTOR + 1] << 8) & 0xff00);
    regist->programCounter = entryPoint;
    
}



char convertCharCode( char code){
    char printChar = '\0';

    for(int i = 0; CHAR_NUM > i ;i++){
        if(convertTbl[i].code == ( code & 0xff)){
            printChar = convertTbl[i].printChar;
            break;
        }
    }
    
    return printChar;
}


void printLine( char *memory ){
    char printChar;
    int flg = false;
    
    for(int i = 0;LINE_SIZE > i;i++){
        if((memory[i] != '\0') &&((memory[i] & 0xff) != 0xa0)){
            //test
            int test = memory[i] & 0xff;
            
            printChar = convertCharCode(memory[i]);
            printf("%c",printChar);
            flg = true;
        }
    }
    
    if(flg){
        printf("\n");
    }
}


void monitor( char *memory ){
    
    printLine(&memory[0x400]);
    printLine(&memory[0x480]);
    printLine(&memory[0x500]);
    printLine(&memory[0x580]);
    printLine(&memory[0x600]);
    printLine(&memory[0x680]);
    printLine(&memory[0x700]);
    printLine(&memory[0x780]);
    printLine(&memory[0x428]);
    printLine(&memory[0x4a8]);
    printLine(&memory[0x528]);
    printLine(&memory[0x5a8]);
    printLine(&memory[0x628]);
    printLine(&memory[0x6a8]);
    printLine(&memory[0x728]);
    printLine(&memory[0x7a8]);
    printLine(&memory[0x450]);
    printLine(&memory[0x4d0]);
    printLine(&memory[0x550]);
    printLine(&memory[0x5d0]);
    printLine(&memory[0x650]);
    printLine(&memory[0x6d0]);
    printLine(&memory[0x750]);
    printLine(&memory[0x7d0]);
            
}


void debugMonitor( t_regist regist, t_statusRegist statusRegist, char *memory ){
    
    int accum = regist.accumulator & 0xff;
    int indeX = regist.indexX & 0xff;
    int indeY = regist.indexY & 0xff;
    int sp = regist.stackPointer & 0xff;
    
    int n = statusRegist.bits.negative & 0x01;
    int z = statusRegist.bits.zero & 0x01;
    int c = statusRegist.bits.carry & 0x01;
    int v = statusRegist.bits.overFlow & 0x01;
    int mem = memory[0x36] & 0xff;
    int mem2 = memory[0x37] & 0xff;
    
    printf("PC :%x\n", regist.programCounter);
    //printf("inst :%d\n", inst.opcode);
    printf("ACUM :%x\n", accum);
    printf("indexX :%x\n", indeX);
    printf("indexY :%x\n", indeY);
    printf("SP :%x\n", sp);
    printf("N :%d\n", n);
    printf("Z :%d\n", z);
    printf("C :%d\n", c);
    printf("V :%d\n", v);
    printf("LOC36 :%d\n", mem);
    printf("LOC37 :%d\n", mem2);
}


int main(int argc, const char * argv[]) {
    t_regist regist = { 0 };
    t_statusRegist statusRegist = { 0 };
    t_inst inst = { 0 };
    
    char memory[MEMORY_SIZE];
    
    int kill = false;
    
    initMemory(memory);
    initRegist( &regist, memory );
    
    while(!kill){
        //interapt
        //getKeyInput(memory);
        
        //debug
        //debugMonitor(regist, statusRegist, memory);
        
        //fetch
        getInst( &regist, memory, &inst );
                
        //execute
        execute(inst, &regist, &statusRegist, memory);
        
        //monitor
        monitor( memory );

    }
    
    return 0;
}
