*derive broad depression from ICD-10 code based on Howard et el. Nature communication. (2018)9:1470. PMID:29662059 

cd \path\to\extracted\input\data

import delimited icd10_main.csv, clear

gen depression_diag_main = .
foreach var of varlist v* {
    replace depression_diag_main = 1 if (substr(`var',1,3) == "F32"  | substr(`var',1,3) == "F33" | substr(`var',1,3) == "F34" | substr(`var',1,3) == "F38" | substr(`var',1,3) == "F39") 
}

keep eid depression_diag_main
export delimited eid depression_diag_main using "icd10_main_depression.csv", replace
  
*******************************************************************************************************************************************  
  
import delimited icd10_second.csv, clear

gen depression_diag_second = .
foreach var of varlist v* {
    replace depression_diag_second = 1 if (substr(`var',1,3) == "F32"  | substr(`var',1,3) == "F33" | substr(`var',1,3) == "F34" | substr(`var',1,3) == "F38" | substr(`var',1,3) == "F39") 
}

keep eid depression_diag_second
export delimited eid  depression_diag_second using "icd10_second_depression.csv", replace
