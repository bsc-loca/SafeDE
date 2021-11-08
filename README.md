# SafeDE - light lockstep hardware module

SafeDE is a small hardware module for light-lockstep

This repository contains the RTL and documentation for the unit.

- The specs found under the `docs` folder.
- RTL of the design (top level and slack handler submodule) can be found in `hdl`.
- `synthesis` contains scripts for early area and frequency evaluation with vivado.
- `tb` contains the vhdl files and the Makefile to launch the test-bench with QuestaSim.
- `API` contains a c library to configure and control SafeDE.
- `ci` contains files for the continuous integration.

