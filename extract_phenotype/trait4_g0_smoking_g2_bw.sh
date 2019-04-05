cd ${HOME}/mini_project3/phenotype

head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep 'eid'

#age at first live birth
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '2754-0.0'

#smoking status
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '20116-0.0'

#age of stopping smoking
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '22507-0.0'

#age started smoking in current smokers
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '3436-0.0'

#age started smoking in former smokers
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '2867-0.0'

#age stopped smoking
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '2897-0.0'

#first child birthweight
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '2744-0.0'

#number of live births
head -n 1 data.11148.csv | sed 's/,/\n/g' | cat -n | grep '2734-0.0'

cut -d',' -f 1,909,6109,6747,1068,939,948,906,903 data.11148.csv > maternal_smoking_women_participants.csv
