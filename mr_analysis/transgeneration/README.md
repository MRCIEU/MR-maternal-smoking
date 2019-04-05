## Transgeneration

We aim to test the association of grandmother (G0) smoking in pregnancy with grandchild (G2) birthweight. 

The only difference here comparing to [proof of principle analysis](https://github.com/MRCIEU/MR-maternal-smoking/blob/master/mr_analysis/README.md) is we need to stratify female participants (G1) based on their own and their mother's smoking status in pregnancy.

Therefore, we need to derive smoking status ("0"=No, "1"=Yes) in pregnancy for those female participants.

### 1. Derive smoking status

To run the code in R, use:

```
source ('Derive_G1_smoke_pregnancy.R')
```

### 2. Transgenerational MR

To run the code in R, use:

```
source ('G0_smoke_G2_birthweight.R')
```

### 3. Difference in differences

We first calculate (1) the difference in the estimates between G0-smoker-G1-smoker and G0-nonsmoker-G1-smoker, and (2) the difference in the estimates between G0-smoker-G1-nonsmoker and G0-nonsmoker-G1-nonsmoker. Then we compare these two differences. To run the code in R, use:

```
source ('difference_in_difference.R')
```