## Extract genotype

The code in this directory shows how to extract rs16969968 genotype from the whole UK Biobank genetic dataset.

### 1. Extract rs16969968 using [qctool](http://www.well.ox.ac.uk/~gav/qctool/index.html)

```
qsub extract_snp_wide_format.sh
```

### 2. Change the format to txt

From step 1, we get genetic data in `.bgen` saved in `${HOME}/mini_project3/data/check/`. 

In step 2, save `gen_to_expected.py` in the same folder with the data first. 

Then run the code in `change_format_to_txt.sh` line by line to get those data saved in a txt file. 


### 3. Convert from wide to long format & Quality control (QC)

Each row is a participant and the column is the genotype of rs16969968. Only includes participants of Whilte British and not genetically related. To run the code in R, use:
```
source ('wide_to_long_plus_QC.R')
```

