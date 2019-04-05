## Extract phenotype
The code in this directory extracts phenotypes from UK Biobank phenotypic dataset for application 16729 using Linux. 

We use `trait1_birth_weight.sh` as an example to expalin how to run the code line by line.

### 1. Change directory to where the data is
```
cd ${HOME}/mini_project3/phenotype
```
### 2. Identify column# of participant ID
```
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep 'eid'
```
### 3. Identify column# of specific outcomes 

You may get a different column# in your own data. That is why the code is run line by line.

```
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '1787-0.0'
```
### 4. Extract phenotypes based on their column# 
```
cut -d',' -f 1,696,5165 data.11148.csv > positive_control.csv
```

### 5. A summary of phenotypes extracted for this study

Smoking-related variables 

|Smoking phenotypes|UK Biobank field ID|
| --- | --- |
|Maternal smoking around birth|1787|
|Smoking status|20116|
|Age started smoking in current smokers|3436|
|Age started smoking in former smokers|2867|
|Age stopped smoking|2897|

#
Outcomes

|Outcomes|UK Biobank field ID|
| --- | --- |
|Birth weight|20022|
|Standing height|50|
|Body mass index (BMI)|21001|
|Forced expiratory volume in 1-second (FEV1), Best measure|20150|
|Forced vital capacity (FVC), Best measure|20151|
|Blood clot, DVT, bronchitis, emphysema, asthma, rhinitis, eczema, allergy diagnosed by doctor|6152|
|Systolic blood pressure, automated reading|4080|
|Systolic blood pressure, manual reading|93|
|Diastolic blood pressure, automated reading|4079|
|Diastolic blood pressure, manual reading|94|
|Age when periods started (menarche)|2714|
|Qualifications|6138|
|Fluid intelligence score|20016, 20191|
|Seen doctor (GP) for nerves, anxiety, tension or depression|2090|
|Seen a psychiatrist for nerves, anxiety, tension or depression|2100|
|Diagnoses - main ICD10|41202|
|Diagnoses - secondary ICD10|41204|
|Happiness|4526|
|Birth weight of first child|2744|

#
Potential confounders 

|Confounder|UK Biobank field ID|
| --- | --- |
|Age at recruitment|21022|
|Age at first live birth|2754|
|Sex|31|
|Townsend deprivation index at recruitment|189|
|Average total household income before tax|738|

