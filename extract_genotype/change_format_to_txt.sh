#convert from bgen to gen for python
cd ${HOME}/mini_project3/data/check
qctool -g smoke.bgen -og smoke.gen

#convert from gen to txt
cat smoke.gen | python gen_to_expected.py > smoke-dosage.txt

cat smoke-dosage.txt | awk '{print NF}' 

#convert sample (participant ID) file to txt format
awk '(NR>2){print $1}' data.chr15.sample > userIds.txt
