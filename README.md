#Tic Tac Toe in Erlang

Project for "Sistemas Operativos I" (Operative Systems 1)

"Facultad de Ciencias Exactas y Agrimensura", Argentina

#Levantar server
Lo primero que debe hacerse desde el bash es ejecutar $epmd -daemon.

Iniciamos el nuevo nodo ejecutando $erl -sname node_name -setcookie cookie (por defecto la cookie que usaremos
en este trabajo es aaa).

Si es el primer nodo que se levanta para actuar como server, entonces se ejecuta start_server(); caso
contrario, debe ejecutarse start_server('working_node').

