#Simulations results are stored in transcript
#Traces of the simulation are stored in vsim.wlf when Questa is launched with the gui (vsim-launch)

hdl=../hdl

#Compiles the files and creates the library safety
compile:
	vlib safety
	mkdir -p safety
	vmap safety safety
	vcom -work safety $(hdl)/lightlock_pkg.vhd $(hdl)/apb_lightlock.vhd $(hdl)/staggering/staggering_handler.vhd
	vcom $(hdl)/lightlock_pkg.vhd $(hdl)/apb_lightlock.vhd $(hdl)/lightlock_pkg.vhd $(hdl)/staggering/staggering_handler.vhd apb_lightlock_tb.vhd inst_count_sim.vhd


#Launches the simulation with the graphical interface
vsim-launch: compile
	vsim -voptargs=+acc apb_lightlock_tb -do "do wave.do" -do "run 21 us"

#Launches the simulation in batch mode
vsim: compile
	vsim -voptargs=+acc apb_lightlock_tb -do "run 21 us" <<! 
    
#Analyzes the results of the simulation
launch-tb : vsim
	@echo "\n\n\n\n\n"
	@errors=`grep 'Errors: 0' transcript` ; \
	echo "$$errors" ; \
	if [ "$$errors" = "" ]; then \
	echo 'TEST FAILED, SOME ERRORS FOUND:' ; \
	grep Error transcript ; \
	else \
	echo 'TEST PASSED WITHOUT ERRORS' ; \
	fi

#Cleans the temporal files
clean:
	rm -rf safety
	rm -rf work
	rm -f modelsim.ini
	rm -f transcript
	rm -f vsim_stacktrace.vstf  
	rm -f vsim.wlf

