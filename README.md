#Tic Tac Toe in Erlang

Project for "Sistemas Operativos I" (Operative Systems 1)

"Facultad de Ciencias Exactas y Agrimensura", Argentina

#Levantar server
Lo primero que debe hacerse desde el bash es ejecutar $epmd -daemon.

Si es el primer nodo que se levanta para actuar como server, entonces se ejecuta start_server('node_name'); caso
contrario, debe ejecutarse start_server('node_name','working_node'). Luego, en el eshell debe ejecutarse
erlang:set_cookie(node(),cookie) (por defecto usaremos la cookie aaa).

