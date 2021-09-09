#
# NOTE: typical usage would be "vivado -mode tcl -source run_synthesis_batch.tcl"
#
# STEP#0: define output directory area.
#
set outputDir ./output
file mkdir $outputDir
#
# STEP#1: setup design sources and constraints
#
read_vhdl -library bsc ../lockstep_pkg.vhd
read_vhdl ../apb_lockstep.vhd  
read_vhdl ../slack/slack_handler.vhd



#
# STEP#2: run synthesis, report utilization and timing estimates, write checkpoint design
#
synth_design -top apb_lockstep 
write_checkpoint -force $outputDir/post_synth
report_timing_summary -file $outputDir/post_synth_timing_summary.rpt
report_power -file $outputDir/post_synth_power.rpt
