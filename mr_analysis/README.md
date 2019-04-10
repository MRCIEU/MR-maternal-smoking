## Gene by Environment MR

Our statistical analyses mainly consist of the following parts:

### 1. Proof of principle analysis

We stratify G1 sample based on whether their mothers (G0) smoked or not during pregnancy and test the association of rs16969968 with offspring (G1) birthweight in each strata. To run the code in R, use:

```
source ('offspring_birthweight.R')
```

### 2. Offspring (G1) outcomes in their later life

We test associations of rs16969968 with 12 offspring (G1) outcomes in their later life. See [offspring_later](https://github.com/MRCIEU/MR-maternal-smoking/tree/master/mr_analysis/offspring_later).

|No.|Outcomes|
|---|---|
|a|height|
|b|body mass index|
|c|lung function - FEV1|
|d|lung function - FVC|
|e|asthma|
|f|systolic blood pressure|
|g|diastolic blood pressure|
|h|age at menarche|
|i|years of education| 
|j|fluid intelligence score|
|k|depression/anxiety|
|l|happiness|

### 3. Grandchildren (G2) birthweight

See [transgeneration](https://github.com/MRCIEU/MR-maternal-smoking/tree/master/mr_analysis/transgeneration)

### 4. Associations of rs16969968 with potential confounders

Potential confounders include **age**, **age at first live birth**(female only), **deprivation index**, [**education**](https://github.com/MRCIEU/MR-maternal-smoking/blob/master/mr_analysis/offspring_later/i_education.R), **income** and **sex**.

```
source ('confounder.R') 
```
### 5. Interactions 

We compare differences in genetic associations between participants (G1) whose mothers (G0) smoked in pregnancy and participants whose mothers did not smoked in pregnancy. To run the code in R, use:

```
source ('test_interaction.R') 
```
### 6. Observational associations for comparison

We calculate associations of maternal (G0) smoking status with participants (G1) smoking status and all outcomes we test. To run the code in R, use:
```
source ('observational.R') 
```
