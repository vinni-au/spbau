for ((i=0; i < 8; i++))
do
	x=`expr $i % 2`
	if [ $x -eq 0 ]
	then
		echo -ne "  "
	fi
	echo -e "\033[47m  \033[0m  \033[47m  \033[0m  \033[47m  \033[0m  \033[47m  \033[0m"
done
