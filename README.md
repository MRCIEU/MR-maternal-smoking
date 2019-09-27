# Proxy gene × environment Mendelian randomization study

This study uses UK Biobank participants' 'smoking heaviness' SNP rs16969968 in gene *CHRNA5* as a proxy of their mother's rs16969968.

This repository accompanies the paper:

Yang Q, et al. A proof of principle proxy gene × environment Mendelian randomization study: Testing causal effects of maternal smoking heaviness during pregnancy. bioRxiv (2019).

## Our analyses mainly have 5 steps:

(1) Extract genotype information

(2) Extract phenotype information

(3) Conduct MR analyses

(4) Visualize MR results

(5) Compare statistical power of G-by-E MR with that of proxy G-by-E MR in the simulation 

## Set enviromental variables

To extract genotype information from UKB shared data using Linux, we set the following environment variable:
```
export UKB_DATA="/path/to/ukb/data"
```

We run all R scripts in Windows by setting the folders containing extracted input data, output results and UKB shared data in R (version 3.5.1) as below:  
```
Sys.setenv(Mydata="/path/to/extracted/input/data")
Sys.setenv(Myresults="/path/to/output/results")
Sys.setenv(UKB_DATA="/path/to/ukb/data")
```
