cd ${HOME}/mini_project3/phenotype

head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#Smoking status
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '20116-0.0'

#Body mass index - constructed from height and weight measured during the initial Assessment Centre visit
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '21001-0.0'

#Forced vital capacity (FVC) - do not use any more
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '3062-0.0'

#Forced expiratory volume in 1-second (FEV1) - do not use any more
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '3063-0.0'

cut -d',' -f 1,6109,6608,3062,3063 data.11148.csv > offspring_bmi_lung.csv
