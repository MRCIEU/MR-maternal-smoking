cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#simple unweighted sum of the number of correct answers given to the 13 fluid intelligence questions
#from UKB assessment centre
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '20016'

#from "online follow-up"
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '20191'

cut -d',' -f 1,5404-5406,6517 data.21753.csv > iq.csv
