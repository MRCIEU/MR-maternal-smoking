cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#Seen doctor (GP) for nerves, anxiety, tension or depression
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '2090'

#Seen a psychiatrist for nerves, anxiety, tension or depression
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '2100'

#Diagnoses - main ICD10
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '41202'

#Diagnoses - secondary ICD10
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '41204'

#save in 3 files
cut -d',' -f 1,859-864 data.21753.csv > depression.csv
cut -d',' -f 1,10231-10610 data.21753.csv > icd10_main.csv
cut -d',' -f 1,10639-11073 data.21753.csv > icd10_second.csv
