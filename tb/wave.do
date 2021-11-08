onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/clk
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/rstn
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/stall2
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/stall1
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/icnt2
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/icnt1
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/error_lightlock
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/apbo_prdata
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/apbi_pwrite
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/apbi_pwdata
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/apbi_psel
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/apbi_penable
add wave -noupdate -expand -group top_apb_lightlock /apb_lightlock_tb/apbi_paddr
add wave -noupdate -expand -group icnt_generator -radix binary /apb_lightlock_tb/icnt_generation/icnt1_o
add wave -noupdate -expand -group icnt_generator -radix binary /apb_lightlock_tb/icnt_generation/icnt2_o
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/core1_ahead_core2
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/core1_ahead_core2
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/enable
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/enable
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/global_enable
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/enable_core1
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/enable_core1
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/enable_core2
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/enable_core2
add wave -noupdate -expand -group lightlock -radix unsigned /apb_lightlock_tb/apb_lightlock_inst/min_staggering
add wave -noupdate -expand -group lightlock -radix unsigned /apb_lightlock_tb/apb_lightlock_inst/max_staggering
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/core1_ahead_core2
add wave -noupdate -expand -group lightlock -radix unsigned /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/n_executed_inst1
add wave -noupdate -expand -group lightlock -radix unsigned /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_executed_inst1
add wave -noupdate -expand -group lightlock -radix decimal /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_executed_inst2
add wave -noupdate -expand -group lightlock -radix decimal /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/n_executed_inst2
add wave -noupdate -expand -group lightlock -radix unsigned /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/instruction_difference
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/stall1_o
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/stall2_o
add wave -noupdate -expand -group lightlock /apb_lightlock_tb/apb_lightlock_inst/r
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_total_cycles
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_total_cycles
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_times_stalled_core2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_times_stalled_core2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_times_stalled_core1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_times_stalled_core1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_min_inst_diff
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_min_inst_diff
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_max_inst_diff
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_max_inst_diff
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_executed_inst2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_executed_inst2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_executed_inst1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_executed_inst1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_enable_core2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_enable_core1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_cycles_stalled_core2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_cycles_stalled_core2
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_cycles_stalled_core1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_cycles_stalled_core1
add wave -noupdate -expand -group statistics /apb_lightlock_tb/apb_lightlock_inst/staggering_handler_inst/r_min_inst_diff
add wave -noupdate -expand -group statistics /apb_lightlock_tb/r_min_inst_diff
add wave -noupdate /apb_lightlock_tb/r_activate_minimum_inst_comp
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {405 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 427
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits us
update
WaveRestoreZoom {0 ns} {2368 ns}
