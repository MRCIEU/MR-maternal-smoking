cd ${HOME}/mini_project3/phenotype

head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#Standing height
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '50-0.0'

#Qualifications
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '6138'

#Age at recruitment
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '21022-0.0'

#Age completed full time education - do not use it any more due to missingness
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '845-0.0'

#Country of birth (UK/elsewhere)
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '1647-0.0'

cut -d',' -f 1,26,3514-3519,6620,441,660 data.11148.csv > height_education.csv
