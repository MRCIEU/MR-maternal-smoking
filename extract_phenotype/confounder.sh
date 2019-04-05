cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#Average total household income before tax
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '738-0.0'

#Townsend deprivation index at recruitment
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '189-0.0'

cut -d',' -f 1,508,304 data.21753.csv > confounder.csv
