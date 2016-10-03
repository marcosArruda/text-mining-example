counter=0
for i in *.pdf
do
count=$((count+1))
	pdftotext -enc "UTF-8" -nopgbrk $i file$count.txt
	data=$(cat file$count.txt  | tr '\n' ' ')
	echo $data > file$count.txt 
done
