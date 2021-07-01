#include <stdio.h>
#include <lockstep.h>
//#define __LOCKSTEP_DEBUG__
#define __RISCV__ 0


// Write lockstep register
static inline __attribute__((always_inline)) void write (unsigned int entry, unsigned int value) {
    volatile unsigned int *p; 
    volatile unsigned int current_value; 
#ifdef __LOCKSTEP_DEBUG__
    printf("\n *** Write register***\n\n");
#endif
    p=(unsigned int*)(LOCKSTEP_ADDR+(entry*4));
    *p=value;
#ifdef __LOCKSTEP_DEBUG__
    printf("address:%x \n",(LOCKSTEP_ADDR+(entry*4)));
    printf("new value :%x \n", value);
    printf("\n *** end Write register ***\n\n");
#endif
}

// Read lockstep register
static inline __attribute__((always_inline)) unsigned int read (unsigned int entry) {
    volatile unsigned int *p; 
    volatile unsigned int value; 
#ifdef __LOCKSTEP_DEBUG__
    printf("\n *** Read register***\n\n");
#endif
    p=(unsigned int*)(LOCKSTEP_ADDR+(entry*4));
    value=*p;
#ifdef __LOCKSTEP_DEBUG__
    printf("address:%x \n",(LOCKSTEP_ADDR+(entry*4)));
    printf("new value :%x \n",value);
    printf("\n *** end Write register ***\n\n");
#endif
    return value;
}

//It enables SafeDE module
//If it is disable neither of the stall signals can be set to 1
void enable_lockstep() {
    unsigned int reg;
    reg = read(LOCKSTEP_CONFIG);
    reg |= (1 << 30);
    write(LOCKSTEP_CONFIG, reg);
}

//It disables SafeDE module
void disable_lockstep() {
    unsigned int reg;
    reg = read(LOCKSTEP_CONFIG);
    reg &= ~(1 << 30);
    write(LOCKSTEP_CONFIG, reg);
}

//It sets minimum and maximum slack (minimum and maximum threshold) 
//Difference between max and min threshold has to be 4 at least
void set_min_max_thresholds(int min_slack, int max_slack) {
    unsigned int reg;
    if (min_slack > max_slack) {
        printf("\nERROR: minimum threshold is bigger than maximum threshold. Lockstep will remain off\n");
    } else if (max_slack-min_slack < 4) {
        printf("\nERROR: difference between maximum and minimum threshold must be bigger than 4. Lockstep will remain off\n");
    } else if (max_slack > 32767) {
        printf("\nERROR: maximum threshols is too big. Biggest possible value is 32767. Lockstep will remain off\n");
    } else if (min_slack < 4) {
        printf("\nERROR: minimum threshold must be bigger than 4. Lockstep will remain off\n");
    } else { 
#ifdef __LOCKSTEP_DEBUG__
    printf("\nLockstep mode is now activated\n");
#endif
    reg = read(LOCKSTEP_CONFIG);
    reg &= 1 << 30;
    reg |= (max_slack << 15 | min_slack);
    write(LOCKSTEP_CONFIG, reg);
    }
}

//It sets minimum slack (minimum threshold only)
//Mximum slack is set to its maximum value (32767) to avoid overflow
void set_min_threshold(int min_slack) {
    unsigned int reg;
    if (min_slack > 32767) {
        printf("\nERROR: minimum threshol is too big. Biggest possible value is 32767. Lockstep will remain off\n");
    } else if (min_slack < 4) {
        printf("\nERROR: minimum threshold must be bigger than 4. Lockstep will remain off\n");
    } else { 
#ifdef __LOCKSTEP_DEBUG__
    printf("\nLockstep mode is now activated\n");
#endif
    reg = read(LOCKSTEP_CONFIG);
    reg &= 1 << 30;
    reg |= min_slack;
    write(LOCKSTEP_CONFIG, reg);
    }
}



//Each core has to call this function right before starting the critical section
//When the lockstep is activated, the corresponding memory position is written. This
//instruction won't be effective immediately if the store goes into the write buffer and
//this can cause that both cores are not perfectly synchronized.
//To avoid this problem, the memory position is read after it is written. For that solution
//to be effective, it must be done in assembly.
//If the preprocessor variable __RISCV__ is set to 1, the assembly code for the RISCV processor
//will be used. If the target core is not RISCV, __RISCV__ must be set to 0. In that case
//cores won't be perfectly synchronized.
void start_criticalSec(int core) {
    int address;
    if (core != 1 && core !=2) {
        printf("\nERROR: The last argument of the function must be 1 or 2.\n");
    } else {
#if __RISCV__ == 0 
        write(core, 1);
        read(core);
#else
        address = (LOCKSTEP_ADDR+((core)*4));
        __asm__ __volatile__ (
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "sw %1, 0(%0) " "\n\t"
        "lw %0, 0(%0) " "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        "nop"  "\n\t"
        ://%0        %1
        : "r"(address), "r"(1)
        );
#endif

#ifdef __LOCKSTEP_DEBUG__
        printf("\nCore %u has entered the critical section.\n", core);
#endif
    }
}


//Each core has to call this function rigth before ending the critical secion
void finish_criticalSec(int core) {
    if (core != 1 && core !=2) {
        printf("\nThe argument of the function must be 1 or 2.\n");
    } else {
        write(core, 0);
#ifdef __LOCKSTEP_DEBUG__
        printf("\nCore %u has left the critical section.\n", core);
#endif
    }
}


//It prints the content of the statistics registers
void report_lockstep(void) {
    unsigned int slack;
    int min_slack;
    int max_slack;
    
    int acc_inst_diff;
    int total_cycles;
    int diff_average;

    acc_inst_diff = read(ACCUMULATED_INST_DIFF);
    total_cycles = read(TOTAL_CYCLES);
    diff_average = (int) (acc_inst_diff/total_cycles);
    

    slack = read(LOCKSTEP_CONFIG);
    min_slack = (slack & 0x00001FFF);
    max_slack = (slack >> 15) & ~(1 << 15);

    printf("\n\n--------------- STARTING LOCKSTEP REPORT: ------------------ \n\n");
    printf("\n\n------------- min_slack: %u, max_slack: %u  ---------------- \n\n", min_slack, max_slack);

    printf("                  Cycles_active: %u\n", total_cycles);
    printf("    Executed_instructions_core1: %u\n", read(EXECUTED_INSTRCTIONS1));
    printf("    Executed_instructions_core2: %u\n", read(EXECUTED_INSTRCTIONS2));
    printf("            Times_stalled_core1: %u\n", read(TIMES_STALLED_CORE1  ));
    printf("            Times_stalled_core2: %u\n", read(TIMES_STALLED_CORE2  ));
    printf("           Cycles_stalled_core1: %u\n", read(CYCLES_STALLED_CORE1 ));
    printf("           Cycles_stalled_core2: %u\n", read(CYCLES_STALLED_CORE2 ));
    printf(" Biggest_instruction_difference: %u\n", read(MAX_INSTRUCTION_DIFF ));
    printf(" Average_instruction_difference: %u\n", diff_average);
    printf("Smallest_instruction_difference: %u\n", read(MIN_INSTRUCTION_DIFF ));
    
    printf("\n\n---------------- END OF LOCKSTEP REPORT -------------------- \n\n");

}

//It sets to 0 all the counters of the lockstep and resets the module
void softreset_lockstep(void) {
    unsigned int reg;
    reg = read(LOCKSTEP_CONFIG);
    reg |= 1 << 31;
    write(LOCKSTEP_CONFIG, reg);
}

