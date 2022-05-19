#include <lightlock_vars.h>

//FUNCTION DECLARATION                                                              //Driver values
void lightlock_softreset(void);                                                     //0x0 
void lightlock_enable(void);                                                        //0x1
void lightlock_min_threshold(int min_staggering);                                   //0x2 0000
void lightlock_min_max_thresholds(int min_staggering, int max_staggering);          //0x3 0000 0000
void lightlock_start_criticalSec(int core);                                         //0x4 0
void lightlock_finish_criticalSec(int core);                                        //0x5 0
void lightlock_disable_lockstep(void);                                              //0x6
void lightlock_report(void);                                                        //0x7
