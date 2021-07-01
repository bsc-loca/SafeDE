//base addres for lockstep on SoC
#define LOCKSTEP_ADDR 0x80000600

// CONFIGURATION REGISTERS
#define         LOCKSTEP_CONFIG             0  //bit 32 soft reset, bit 31 global enable, bits 30-15 max slack, bits 15-0 min slack
#define         CRIT_SECTION1               1  //bit 0 indicates the start of the critical section in the core1
#define         CRIT_SECTION2               2  //bit 0 indicates the start of the critical section in the core2

// STATISTICS REGISTERS
#define         DATA_REGS_START             3
#define         TOTAL_CYCLES                3
#define         EXECUTED_INSTRCTIONS1       4
#define         EXECUTED_INSTRCTIONS2       5
#define         TIMES_STALLED_CORE1         6
#define         TIMES_STALLED_CORE2         7
#define         CYCLES_STALLED_CORE1        8
#define         CYCLES_STALLED_CORE2        9
#define         MAX_INSTRUCTION_DIFF        10
#define         ACCUMULATED_INST_DIFF       11
#define         MIN_INSTRUCTION_DIFF        12

