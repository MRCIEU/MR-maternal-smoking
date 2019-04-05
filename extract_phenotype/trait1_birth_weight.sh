cd ${HOME}/mini_project3/phenotype

head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#Maternal smoking around birth
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '1787-0.0'

#Birth weight(participants theirselves)
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '20022-0.0'

cut -d',' -f 1,696,5165 data.11148.csv > positive_control.csv
