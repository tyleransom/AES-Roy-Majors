# AES-Roy-Majors
This repository provides replication files for the paper "Selective Migration, Occupational Choice, and the Wage Returns to College Majors" written by Tyler Ransom and published in *Annals of Economics and Statistics* in 2021. The paper is available [here](https://www.jstor.org/stable/10.15609/annaeconstat2009.142.0045#metadata_info_tab_contents).

## System requirements
- R version 4.0.x
- packages called at the top of `Data/import_and_clean19.R` and `Analysis/analysis_all.R`
- A machine with at least 32GB of RAM, four processors and at least 50GB of hard drive space

## Data
- Data comes from [IPUMS USA](https://usa.ipums.org)
- In accordance to IPUMS licensing requirements, I do not post the raw data here
- For a list of variables included in the raw data extract, see `Data/usa_00045.xml`
- To obtain the raw data, you will need to create an account at [IPUMS USA](https://usa.ipums.org) and then create an extract using the variables listed in `Data/usa_00045.xml`
- Save this raw data file in the `Data` folder

## Steps to reproduce the results
1. Obtain data as described directly above
2. Run `Data/import_and_clean19.R` 
3. Run `Analysis/analysis_all.R`
4. Run the following files in `Simulations/`:
    - `simulation.R`
    - `simulationCorrEps.R`
    - `simulationCorrEpsCorrEta.R`
    - `simulation1e3.R`
    - `simulation1e3CorrEps.R`
    - `simulation1e3CorrEpsCorrEta.R`
    - and then `cometogether.R`
    - (See `Simulations/README.md` for further details)

**WARNING:** it may take up to 3 days for the results to complete. This time may be reduced if you have more RAM and more processors. (The majority of computation time is spent on bootstrapping standard errors; this time can be reduced by increasing the number of workers on line 313 of `Analysis/table3splitBoot.R`.)
