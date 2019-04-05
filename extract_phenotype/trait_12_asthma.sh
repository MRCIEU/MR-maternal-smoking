cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#asthma 
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '6152-0.0'

#age when asthma was diagonised - do not use any more
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '3786-0.0'

cut -d',' -f 1,3818,1252 data.21753.csv > asthma.csv
