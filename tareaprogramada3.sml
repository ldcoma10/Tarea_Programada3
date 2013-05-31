 
 (* Función que lee del archivo y guarda el contenido en una lista*)
 (* Entradas: el nombre del archivo por leer
	Salidas : Lista con las lineas leída
	Nota: Cada elemento de la lista que es una línea del archivo leído*)
	
 fun readlist (archivo : string) = 
	let val ins = TextIO.openIn archivo 
	fun loop ins = case TextIO.inputLine ins of SOME line => 
	line :: loop ins | NONE => 
	[] in loop ins before TextIO.closeIn ins end ; 
	
	

(*Función que lee el archivo char por char *)
(*Entradas: la cabeza de la lista leida que sería un string y 
			una lista de tuplas vacía *)
(*Salidas:  una lista de tuplas de todos los numeros encontrados 
			que podría tener una formato parecido al de fechas*)
			
fun leerarch(leer:string list,tupla: (int*int*int) list)=
	if null leer
	then
		rev tupla
	else
		let fun leeraux(separado:char list,leer:string list,tupla: (int*int*int) list)=
		if null separado
		then
			leerarch(tl (leer),tupla)
		else 
			let fun leerarchaux(sep: char list,tupl: (int*int*int) list,leer:string list)=
			if not(Char.isDigit(hd(sep)))
			then
				leeraux(tl(sep),leer,tupl)
			else
				let fun formartup(sep:char list,cont:int,tuplaunir:(int*int*int),tupl:(int*int*int) list,m:string,leer:string list)=
					if Char.isDigit(hd(sep))
					then
						if cont=1
						then
							formartup(tl(sep),cont,(((#1 tuplaunir*10)+(ord(hd(sep))-48)),#2 tuplaunir,#3 tuplaunir),tupl,Char.toString(hd(tl(sep))),leer)
						else if cont=2
						then
							formartup(tl(sep),cont,(#1 tuplaunir,((#2 tuplaunir*10)+(ord(hd(sep))-48)),#3 tuplaunir),tupl,Char.toString(hd(tl(sep))),leer)
						else
							formartup (tl(sep),cont,(#1 tuplaunir,#2 tuplaunir,((#3 tuplaunir*10)+(ord(hd(sep))-48))),tupl,Char.toString(hd(tl(sep))),leer)
					else if m="/"
					then 
						formartup(tl(sep),cont+1,tuplaunir,tupl,Char.toString(hd(tl(sep))),leer)
					else
						leeraux(tl(sep),leer,tuplaunir::tupl)
				
				in
				formartup(sep,1,(0,0,0),tupl,Char.toString(hd(sep)),leer)
				end
			in
			leerarchaux(separado,tupla,leer)
			end
		in 
		leeraux(explode(hd(leer)),leer,tupla)
		end;
		

(*Función que valida una fecha *)
(*Entradas: una tupla de enteros que sería la cabeza de la lista que se retornó la función anterior*)
(*Salidas:  devuelve un true si es válida y sino devuelve un false*)		
		
fun validarfecha(x:int*int*int)=
	if #1x<31 andalso #1x>0 andalso #2x<13 andalso #2x>0 andalso #3x>0
	then
		if #2x=2
		then
			if #1x<30 
			then 
				if #1x=29
				then
					if (#3x mod 4)=0 andalso not((#3x mod 100)=0)
					then
						true
					else
						false		
				else
					true
			else
				false
		else if #2x=4 orelse #2x=6 orelse #2x=9 orelse #2x=11
		then
			if #1x<31
			then
				true
			else 
				false
		else
			true
	else
		false;

		
(* Funcion que cuenta la cantidad de dias en una fecha 
	En este tipo de función devuelva tanto los años,meses en días*) 
fun contarDias (fecha1:(int*int*int) ) =
	let	
    val sumadias1 = ((#1 fecha1 )+ ((#2 fecha1) * 30) + ((#3 fecha1) * 365));    
    in 
    sumadias1
    end;
    
(* Funcion que pasa la lista de fechas a lista de dias *) 
fun fechas_A_dias(lista:(int*int*int) list)=
    if null lista then []
	else  contarDias (hd lista):: fechas_A_dias( tl lista  );
 
(* Funcion de ordenamiento quicksort *) 
fun quicksort [] = 
	[] | quicksort (h::t) = 
  quicksort (List.filter (fn x => x < h) t) @ h :: quicksort (List.filter (fn x => x >= h) t);
  
val lista_meses = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"]

(* Funcion que obtiene el nombre correspondiente a un número de mes *) 
fun nombre_mes (lista_meses : string list, n: int) =
    if null lista_meses then ""
    else if n = 1 then hd lista_meses
    else nombre_mes(tl lista_meses, n-1);
    
(* Imprime la fecha en palabras *)
fun imprimir_fecha (fecha : int * int * int) =
     (Int.toString (#1 (fecha))^ " de " 
     ^ nombre_mes(lista_meses, #2 (fecha)) ^ " de " 
     ^ (Int.toString (#3 (fecha))) ^ "\n");
     
     
(* Función que se encarga de crear una lista con las fechas válidas *)
fun validas(tuplagenerada:(int*int*int)list,tuplavalida:(int*int*int)list )=
	if null tuplagenerada
	then
		rev tuplavalida	
	else
		let val m=validarfecha(hd(tuplagenerada))
			in
			if m
			then
				validas(tl(tuplagenerada),hd(tuplagenerada)::tuplavalida)
			else
				validas(tl(tuplagenerada),tuplavalida)
		end;


(* Funciones para imprimir las fechas  *)   
fun imprimir_aux (num: int , lista: (int*int*int) list )=
	if num= contarDias(hd lista)
		then
			imprimir_fecha(hd(lista))
	else
			imprimir_aux(num, tl lista)
	
fun imprimir(enteros: int list, lista:(int*int*int) list)=	
	if null enteros
		then 
		true
	else
	let val x=
		print(  imprimir_aux(hd enteros, lista) );
		in  imprimir(tl enteros, lista)
		end;

		
		
(* Direccion del archivo a leer, o simplemente el nombre si el
		archivo se encuentra en la misma carpeta que este*)
		
	fun extraer (string, chars) = 
		let fun aux c =
			if String.isSubstring (str c) chars then
			  ""
			else
			  str c
		in
		  String.translate aux string
		end;
		
print("Digite el nombre del archivo a utilizar \n");
val x= TextIO.inputLine TextIO.stdIn;

fun foo(NONE) = ""
  | foo(SOME a) = a;
  
 val m= foo x
 val archivo= extraer(m,"\n");
 val leido = readlist(archivo);
 val tuplagenerada=leerarch(leido,[(0,0,0)]);
 val listafechasvalidas=validas(tl(tuplagenerada),[(0,0,0)]);
 val listaordenada=quicksort(fechas_A_dias(tl(listafechasvalidas)));
 val main=imprimir(listaordenada, listafechasvalidas); 
