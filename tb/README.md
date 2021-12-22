# Test-bench

This folder contains all the files needed to execute a test bench for the SafeDE module. The file `apb_lightlock_tb.vhd` is the top VHDL design. In this file the top of SafeDE is instantiated and all its inputs are simulated.

In the `inst_count_sim.vhd` file, a component that randomly generates the `inct` inputs of SafeDE is defined. All the statistics that the SafeDE module calculates are also derived in the top `apb_lightlock_tb.vhd`, and later they are compared with those read from the SafeDE module through the APB bus. 

The test-bench passes provided that all the results read from the bus coincide with those calculated in the top. And also if the staggering keeps at every moment within the expected limits.



---

### Makefile

The compilation and execution of the test is controlled by means of a Makefile. This Makefile has 4 recipes:

* **compile:** It creates the libraries work and safety and compiles all the VHDL files.
* **vsim-launch:** It compiles and launches the simulation in the QuestaSim graphical user interface.
* **vsim:** It compiles and runs the simulation in batch mode.
* **launch-tb:** It compiles, launches the simulation in batch mode and analyze the results of the test-bench.
* **clean:** It removes temporal files.



To finish the testbench, we use the stop procedure which is imported from the STD.ENV package. To work, the testbench has to be compiled in VHDL-2008 or newer. If this is not possible, the time of the simulation can be controlled executing `vsim -voptargs=+acc apb_lightlock_tb -do "run simulation_time"` when launching the simulation.