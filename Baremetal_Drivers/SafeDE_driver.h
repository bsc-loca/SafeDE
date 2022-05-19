#include <SafeDE_vars.h>

//FUNCTION DECLARATION                                                              //Driver values
void SafeDE_softreset(void);                                                     //0x0 
void SafeDE_enable(void);                                                        //0x1
void SafeDE_min_threshold(int min_staggering);                                   //0x2 0000
void SafeDE_min_max_thresholds(int min_staggering, int max_staggering);          //0x3 0000 0000
void SafeDE_start_criticalSec(int core);                                         //0x4 0
void SafeDE_finish_criticalSec(int core);                                        //0x5 0
void SafeDE_disable(void);                                                       //0x6
void SafeDE_report(void);                                                        //0x7
