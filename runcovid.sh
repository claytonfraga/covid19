
cd ~/MEGA/Projetos/coronavirus/dados/datasets/git/

cp ../full_data.csv .
cp ../../exp_Brazil_2020-03-* .

git add *

git commit -m "Update  COVID-19 datat"
git push -u origin master
