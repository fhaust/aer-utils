


set terminal qt persist

#set pm3d map

list = system('ls '.filename.'*')
#print(list)

splot for [file in list] file using 1:2:3

pause -1
/*reread*/
