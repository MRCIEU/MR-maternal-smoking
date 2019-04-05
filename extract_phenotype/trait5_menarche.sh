cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#age at menarche
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '2714-0.0'

cut -d',' -f 1,988 data.21753.csv > menarche.csv
