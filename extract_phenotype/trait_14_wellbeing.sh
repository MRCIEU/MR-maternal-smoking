#we extract the same variables used in Wootton et al. BMJ 2018

cd ${HOME}/mini_project3/phenotype

head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#ACE touchscreen question "In general how happy are you?"
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4526-0.0'

#Family relationship satisfaction
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4559-0.0'

#Financial situation satisfaction
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4581-0.0'

#Friendships satisfaction
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4570-0.0'

#Health satisfaction
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4548-0.0'

#Work/job satisfaction
head -n 1 data.21753.csv | sed 's/,/\n/g' | cat -n | grep '4537-0.0'

cut -d',' -f 1,1840,1849,1843,1846,1852,1855 data.21753.csv > wellbeing.csv
