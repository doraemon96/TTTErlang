#Tic Tac Toe in Erlang

Project for "Sistemas Operativos I" (Operative Systems 1)

"Facultad de Ciencias Exactas y Agrimensura", Argentina

#Levantar server
Lo primero que debe hacerse desde el bash es ejecutar $epmd -daemon.

Iniciamos el nuevo nodo ejecutando $erl -name port@ip -setcookie cookie (por defecto la cookie que usaremos
en este trabajo es aaa). 

Si es el primer nodo que se levanta para actuar como server ejecutar:

1- server_TTT:init(port).

Caso contrario seguir la siguiente secuencia:

1- mnesia:start().

2- server_TTT:init(port, 'working_node').

#Conectar cliente
Debe tenerse conocimiento de la dirección ip de un server (server principal) que esté escuchando en el puerto 8000,
y acto seguido se ejecuta client("ip_host").

#Cliente
Generamos automáticamente el CmdId como {username, nro}.

