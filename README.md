#Tic Tac Toe in Erlang

Project for "Sistemas Operativos I" (Operative Systems 1)

"Facultad de Ciencias Exactas y Agrimensura", Argentina

#Levantar server
Lo primero que debe hacerse desde el bash es ejecutar $epmd -daemon.

Iniciamos el nuevo nodo ejecutando $erl -name port@ip -setcookie cookie (por defecto la cookie que usaremos
en este trabajo es aaa).

Si es el primer nodo que se levanta para actuar como server, entonces se ejecuta spawn(server_TTT, start_server, [port]); caso
contrario, debe ejecutarse start_server(server_TTT, start_server, ['working_node', port]).

