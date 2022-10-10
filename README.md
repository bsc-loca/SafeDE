# SafeDE - light lockstep hardware module
SafeDE (Safe Diversity Enforcer) is hardware module provides light-lockstep support by means of a non-intrusive and flexible hardware module that preserves staggering across cores running redundant threads, thus bringing time diversity to avoid common cause failures.

For more information please check the `docs` directory, in particular the [main.pdf](docs/main.pdf)

## Reference

If you are using the SafeDE IP for an academic publication, please cite the following papers:

F. Bas, S. Alcaide, G. Cabo, P. Benedicte and J. Abella, "SafeDE: A Low-Cost Hardware Solution to Enforce Diverse Redundancy in Multicores," in IEEE Transactions on Device and Materials Reliability, vol. 22, no. 2, pp. 111-119, June 2022, doi: 10.1109/TDMR.2022.3156799.

```
@ARTICLE{9729773,
  author={Bas, Francisco and Alcaide, Sergi and Cabo, Guillem and Benedicte, Pedro and Abella, Jaume},
  journal={IEEE Transactions on Device and Materials Reliability}, 
  title={SafeDE: A Low-Cost Hardware Solution to Enforce Diverse Redundancy in Multicores}, 
  year={2022},
  volume={22},
  number={2},
  pages={111-119},
  doi={10.1109/TDMR.2022.3156799}}
```

F. Bas et al., "SafeDE: a flexible Diversity Enforcement hardware module for light-lockstepping," 2021 IEEE 27th International Symposium on On-Line Testing and Robust System Design (IOLTS), 2021, pp. 1-7, doi: 10.1109/IOLTS52814.2021.9486715.

```
@INPROCEEDINGS{9486715,
  author={Bas, Francisco and Alcaide, Sergi and Lorenzo, Ruben and Cabo, Guillem and Gil, Guillermo and Sala, Oriol and Mazzocchetti, Fabio and Trilla, David and Abella, Jaume},
  booktitle={2021 IEEE 27th International Symposium on On-Line Testing and Robust System Design (IOLTS)}, 
  title={SafeDE: a flexible Diversity Enforcement hardware module for light-lockstepping}, 
  year={2021},
  pages={1-7},
  doi={10.1109/IOLTS52814.2021.9486715}}
```

## Repo Organization

This repository contains the RTL and documentation for the unit.

- The specs found under the `docs` folder.
- RTL of the design (top level and slack handler submodule) can be found in `hdl`.
- `synthesis` contains scripts for early area and frequency evaluation with vivado.
- `tb` contains the vhdl files and the Makefile to launch the test-bench with QuestaSim.
- `Baremetal_Drivers` contains a c library to configure and control SafeDE in a baremetal system.
- `ci` contains files for the continuous integration.

