cd ${HOME}/mini_project3/phenotype

head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#Forced vital capacity (FVC), Best measure
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '20151-0.0'

#Forced expiratory volume in 1-second (FEV1), Best measure
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '20150-0.0'

cut -d',' -f 1,6212,6211 data.11148.csv > lung_function_best_measures.csv
