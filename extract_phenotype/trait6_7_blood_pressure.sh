cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#systolic blood pressure
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4080'
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '93'

#diastolic blood pressure
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4079'
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '94'

cut -d',' -f 1,1318,1319,254,255,1312,1313,260,261 data.21753.csv > bp.csv
