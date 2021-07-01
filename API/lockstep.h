#include <lockstep_vars.h>

//FUNCTION DECLARATION
void enable_lockstep();
void disable_lockstep();
void set_min_threshold(int min_slack);
void set_min_max_thresholds(int min_slack, int max_slack);
void start_criticalSec(int core);
void finish_criticalSec(int core);
void report_lockstep(void);
void softreset_lockstep(void);
