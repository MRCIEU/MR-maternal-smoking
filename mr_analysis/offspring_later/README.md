## Gene by environment MR of offspring (G1) outcomes in their later life

The only difference between offspring outcomes in their later life and our [proof of principle analysis](https://github.com/MRCIEU/MR-maternal-smoking/blob/master/mr_analysis/README.md) is that we need to further stratify G1 sample on their own smoking status. 


### 1. Run MR for each outcome

To run the code in R, use:

```
source ('a_height.R') 
source ('b_BMI.R')
source ('c_FEV1_d_FVC.R')
source ('e_asthma.R')
source ('f_SBP_g_DBP.R')
source ('h_menarche.R')
source ('i_education.R')
source ('j_intelligence.R')
source ('k_depression.R')
source ('l_wellbeing.R')
```

For `k_depression.R`, we need to define cases and controls using ICD-10 first. To run the code in STATA, use:

```
do k_depression_derive_from_ICD.do
```


### 2. Test for interaction

See [interaction](https://github.com/MRCIEU/MR-maternal-smoking/blob/master/mr_analysis/README.md)

### 3. Sensitivity analyses
We conduct sensitivity analyses for height and age at menarche, because participants (G1) may start smoking after achieving their adulthood height or getting their period.

See [sensitivity analyses](https://github.com/MRCIEU/MR-maternal-smoking/tree/master/mr_analysis/offspring_later/sensitivity_analyses) 
