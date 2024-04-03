let rec sommeT n acc=
	if n=0 
		then acc 
	else somme (n-1) (acc+n);;

let rec somme n=
	if n=0
		then 0
	else
		n + somme (n-1);;