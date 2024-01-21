% Henrique Caroco, 103860

% extrai_ilhas_linha:

% extrai_ilhas_linha_aux(N_L, Linha, Ilhas_, Indice, Ilhas):
% Ilhas_ - Corresponde a lista das ilhas, ate a posicao Indice da linha.

cria_ilhas(N_L, Pontes, Indice, Ilha) :- Ilha = [ilha(Pontes, (N_L, Indice))].

extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha_aux(N_L, Linha, [], 1, Ilhas).

extrai_ilhas_linha_aux(_, [], Ilhas_, _, Ilhas) :- Ilhas = Ilhas_.

extrai_ilhas_linha_aux(N_L, [0|Resto], Ilhas_, Indice, Ilhas) :- plus(Indice, 1, Indices), 
    extrai_ilhas_linha_aux(N_L, Resto, Ilhas_, Indices, Ilhas).

extrai_ilhas_linha_aux(N_L, [X|Resto], Ilhas_, Indice, Ilhas) :- 
    cria_ilhas(N_L, X, Indice, Ilha), append(Ilhas_, Ilha, Ilhas2), plus(Indice, 1, Indices), 
    extrai_ilhas_linha_aux(N_L, Resto, Ilhas2, Indices, Ilhas).


% ilhas: 

% ilhas_aux(Puz, Ilhas_, Indice, Ilhas) - Aplica extrai_ilhas_linha a cada linha do puzzle. 
% Ilhas_ - Corresponde a lista das ilhas, ate a linha Indice do puzzle.


ilhas(Puz, Ilhas) :- ilhas_aux(Puz, [], 1, Ilhas).

ilhas_aux([], Ilhas_, _, Ilhas) :- Ilhas = Ilhas_.
ilhas_aux([Linha|Resto], Ilhas_, Indice, Ilhas) :- 
    extrai_ilhas_linha(Indice, Linha, Linha_), append(Ilhas_, Linha_, Ilhass), 
    plus(Indice, 1, Indices), ilhas_aux(Resto, Ilhass, Indices, Ilhas).


% vizinhas: 

% mesma_linha e mesma_coluna: 
% Ilhas na mesma linha e coluna da ilha pretendida (tambem incluida), por ordem.

mesma_linha_aux(ilha(_, (B, _)), ilha(_, (Y, _))) :- B = Y.
mesma_coluna_aux(ilha(_, (_, C)), ilha(_, (_, Z))) :- C = Z.

mesma_linha(Ilhas, Ilha, Lista) :- include(mesma_linha_aux(Ilha), Ilhas, Lista).
mesma_coluna(Ilhas, Ilha, Lista) :- include(mesma_coluna_aux(Ilha), Ilhas, Lista).


% antes_coluna, depois_coluna, antes_linha e depois_linha:
% Retiram da linha e coluna da ilha as suas vizinhas, caso existam.

antes_coluna(Coluna, Ilha, Resultado, Resultado_) :- Coluna = [Ilha] ->  Resultado_ = []; 
    Coluna = [Ilha|_] -> Resultado_ = []; nth1(Indice, Coluna, Ilha), Indice_ is Indice - 1, 
    nth1(Indice_, Coluna, Resultado), Resultado_ = [Resultado].
depois_coluna(Coluna, Ilha, Resultado, Resultado_) :- Coluna = [Ilha] ->  Resultado_ = []; 
    length(Coluna, Length_C), nth1(Length_C, Coluna, Ult_Ilha_C), 
    Ult_Ilha_C = Ilha -> Resultado_ = []; nth1(Indice, Coluna, Ilha), Indice_ is Indice + 1, 
    nth1(Indice_, Coluna, Resultado), Resultado_ = [Resultado].

antes_linha(Linha, Ilha, Resultado, Resultado_) :- Linha = [Ilha] ->  Resultado_ = []; 
    Linha = [Ilha|_] -> Resultado_ = []; nth1(Indice, Linha, Ilha), Indice_ is Indice - 1,
    nth1(Indice_, Linha, Resultado), Resultado_ = [Resultado].
depois_linha(Linha, Ilha, Resultado, Resultado_) :- Linha = [Ilha] ->  Resultado_ = []; 
    length(Linha, Length_L), nth1(Length_L, Linha, Ult_Ilha_L), 
    Ult_Ilha_L = Ilha -> Resultado_ = []; nth1(Indice, Linha, Ilha), Indice_ is Indice + 1, 
    nth1(Indice_, Linha, Resultado), Resultado_ = [Resultado].

vizinhas(Ilhas, Ilha, Vizinhas) :- mesma_linha(Ilhas, Ilha, Linha), 
    mesma_coluna(Ilhas, Ilha, Coluna), antes_coluna(Coluna, Ilha, _, Antes_C), 
    depois_coluna(Coluna, Ilha, _, Depois_C), antes_linha(Linha, Ilha, _, Antes_L), 
    depois_linha(Linha, Ilha, _, Depois_L), append(Antes_C, Antes_L, Vizinhas_1), 
    append(Depois_L, Depois_C, Vizinhas_2), append(Vizinhas_1, Vizinhas_2, Vizinhas). 


% estado:

% estado_aux utiliza a funcao vizinhas para cada ilha.

estado_aux([], _, Estado, Estado_final) :- Estado_final = Estado.
estado_aux([Ilha|Resto], Ilhas, Estado, Estado_final) :- vizinhas(Ilhas, Ilha, Vizinhas), 
    Estado_Ilha = [[Ilha, Vizinhas, []]], append(Estado, Estado_Ilha, Estado_), 
    estado_aux(Resto, Ilhas, Estado_, Estado_final).

estado(Ilhas, Estado) :- estado_aux(Ilhas, Ilhas, [], Estado).


% posicoes_entre:

% posicoes_entre verifica se as ilhas estao na mesma linha ou coluna;

% posicoes_linha e posicoes_coluna verificam ambas se as posicoes estao corretamente
% ordenadas, caso contrario e usada a funcao, com as ilhas em ordem invertida.

posicoes_linha(Linha, Inicio, Fim, Posicoes_, Posicoes) :- Inicio > Fim -> posicoes_linha(Linha, Fim, Inicio, Posicoes_, Posicoes); 
    plus(Inicio, 1, Coluna), Coluna = Fim ->  Posicoes = Posicoes_; 
    plus(Inicio, 1, Coluna), Posicao = [(Linha, Coluna)], append(Posicoes_, Posicao, Posicoes2), 
    posicoes_linha(Linha, Coluna, Fim, Posicoes2, Posicoes).

posicoes_coluna(Coluna, Inicio, Fim, Posicoes_, Posicoes) :- Inicio > Fim -> posicoes_coluna(Coluna, Fim, Inicio, Posicoes_, Posicoes); 
    plus(Inicio, 1, Linha), Linha = Fim ->  Posicoes = Posicoes_; 
    plus(Inicio, 1, Linha), Posicao = [(Linha, Coluna)], append(Posicoes_, Posicao, Posicoes2), 
    posicoes_coluna(Coluna, Linha, Fim, Posicoes2, Posicoes).

posicoes_entre((Linha1, Coluna1), (Linha2, Coluna2), Posicoes) :- =(Linha1, Linha2) ->  posicoes_linha(Linha1, Coluna1, Coluna2, [], Posicoes);
    =(Coluna1, Coluna2) ->  posicoes_coluna(Coluna1, Linha1, Linha2, [], Posicoes). 


% cria_ponte:

cria_ponte_aux_linha((Linha1, Coluna1), (Linha2, Coluna2), Ponte) :- 
    Coluna1 < Coluna2 ->  Ponte = ponte((Linha1, Coluna1), (Linha2, Coluna2)); 
    Coluna1 > Coluna2 ->  Ponte = ponte((Linha2, Coluna2), (Linha1, Coluna1)).

cria_ponte_aux_coluna((Linha1, Coluna1), (Linha2, Coluna2), Ponte) :- Linha1 < Linha2 ->  
    Ponte = ponte((Linha1, Coluna1), (Linha2, Coluna2)); 
    Linha1 > Linha2 ->  Ponte = ponte((Linha2, Coluna2), (Linha1, Coluna1)).

cria_ponte((Linha1, Coluna1), (Linha2, Coluna2), Ponte):- 
    =(Linha1, Linha2) ->  cria_ponte_aux_linha((Linha1, Coluna1), (Linha2, Coluna2), Ponte);
    =(Coluna1, Coluna2) ->  cria_ponte_aux_coluna((Linha1, Coluna1), (Linha2, Coluna2), Ponte). 


% caminho_livre:

% devolve true se a lista das posicoes entre Pos1 e Pos2 e a lista das posicoes 
% entre I e Vz nao tiverem nenhuma posicao em comum (devolve true tambem se 
% a posicao de I ou Vz for igual a Pos1 ou Pos2).

not_member(Posicoes, Posicao) :- \+ member(Posicao, Posicoes).

caminho_livre(Pos_I, _, _, ilha(_, Pos_I), _).
caminho_livre(Pos_Vz, _, _, _, ilha(_, Pos_Vz)).
caminho_livre(_, Pos_I, _, ilha(_, Pos_I), _).
caminho_livre(_, Pos_Vz, _, _, ilha(_, Pos_Vz)).

caminho_livre(_, _, Posicoes, ilha(_, Pos_I), ilha(_, Pos_Vz)) :- posicoes_entre(Pos_I, Pos_Vz, Posicoes2), 
    maplist(not_member(Posicoes), Posicoes2).


% actualiza_vizinhas_entrada:

% Recursivamente verifica todas as vizinhas, atraves de caminho_livre, 
% adicionando a Lista_Aux aquelas que continuam a ser vizinhas.

actualiza_vizinhas_aux(_, _, _, [Ilha, [], Pontes], Lista_Aux, Nova_Entrada) :- Nova_Entrada = [Ilha, Lista_Aux, Pontes].

actualiza_vizinhas_aux(Pos1, Pos2, Posicoes, [Ilha, [Ultima_Vizinha], Pontes], Lista_Aux, Nova_Entrada) :- 
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, Ultima_Vizinha) ->  
    append(Lista_Aux, [Ultima_Vizinha], Lista_Aux_), Nova_Entrada = [Ilha, Lista_Aux_, Pontes]; 
    Nova_Entrada = [Ilha, Lista_Aux, Pontes].

actualiza_vizinhas_aux(Pos1, Pos2, Posicoes, [Ilha, [Vizinha|Resto], Pontes], Lista_Aux, Nova_Entrada) :- 
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha) ->  append(Lista_Aux, [Vizinha], Lista_Aux_), 
    actualiza_vizinhas_aux(Pos1, Pos2, Posicoes, [Ilha, Resto, Pontes], Lista_Aux_, Nova_Entrada); 
    actualiza_vizinhas_aux(Pos1, Pos2, Posicoes, [Ilha, Resto, Pontes], Lista_Aux, Nova_Entrada).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, [Vizinha|Resto], Pontes], Nova_Entrada) :- 
    actualiza_vizinhas_aux(Pos1, Pos2, Posicoes, [Ilha, [Vizinha|Resto], Pontes], [], Nova_Entrada).
    
    
% actualiza_vizinhas_apos_pontes:

% Aplica a funcao actualiza_vizinhas_entrada a cada entrada do estado.

actualiza_vizinhas_apos_pontes_aux([], _, _, _, Estado, Novo_estado) :- Novo_estado = Estado.

actualiza_vizinhas_apos_pontes_aux([Entrada|Resto], Pos1, Pos2, Pos_Entre, Estado, Novo_estado) :- 
    actualiza_vizinhas_entrada(Pos1, Pos2, Pos_Entre, Entrada, Nova_Entrada), 
    append(Estado, [Nova_Entrada], Estado_), actualiza_vizinhas_apos_pontes_aux(Resto, Pos1, Pos2, Pos_Entre, Estado_, Novo_estado).

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :- posicoes_entre(Pos1, Pos2, Pos_Entre), 
    actualiza_vizinhas_apos_pontes_aux(Estado, Pos1, Pos2, Pos_Entre, [], Novo_estado).


% ilhas_terminadas:    
 
% Se N_pontes nao for X e Pontes nao tiver N_pontes de comprimento, adiciona
% a ilha a Ilhas_term_aux.

ilhas_terminadas(Estado, Ilhas_term) :- ilhas_terminadas_aux(Estado, [], Ilhas_term).

ilhas_terminadas_aux([], Ilhas_term_aux, Ilhas_term) :- Ilhas_term = Ilhas_term_aux.
ilhas_terminadas_aux([[ilha(N_pontes, Pos), _, Pontes] | Resto], Ilhas_term_aux, Ilhas_term) :- 
    \+ N_pontes = 'X', length(Pontes, N_pontes) ->  Ilha_term = [ilha(N_pontes, Pos)], 
    append(Ilhas_term_aux, Ilha_term, Ilhas_term_aux_), 
    ilhas_terminadas_aux(Resto, Ilhas_term_aux_, Ilhas_term);
    ilhas_terminadas_aux(Resto, Ilhas_term_aux, Ilhas_term).


% tira_ilhas_terminadas_entrada:

% Sao retiradas as ilhas terminadas com recurso ao metapredicado exclude,
% retira_vizinhas verifica se cada vizinha esta ou nao em Ilhas_term.

retira_vizinhas(Lista, Ilha) :- nth1(_, Lista, Ilha).
tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], Nova_entrada) :- 
    exclude(retira_vizinhas(Ilhas_term), Vizinhas, Vizinhas_), Nova_entrada = [Ilha, Vizinhas_, Pontes].


% tira_ilhas_terminadas:

% Aplica tira_ilhas_terminadas_entrada a cada entrada do estado.

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :- maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


% marca_ilhas_terminadas_entrada:

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N_pontes, Pos), Vizinhas, Pontes], Nova_entrada):-
    nth1(_, Ilhas_term, ilha(N_pontes, Pos)) ->  Nova_entrada = [ilha('X', Pos), Vizinhas, Pontes]; 
    Nova_entrada = [ilha(N_pontes, Pos), Vizinhas, Pontes].


% marca_ilhas_terminadas:

% Aplica marca_ilhas_terminadas_entrada a cada entrada do Estado.

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :- 
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


% trata_ilhas_terminadas:

trata_ilhas_terminadas(Estado, Novo_estado) :- ilhas_terminadas(Estado, Terminadas),
    tira_ilhas_terminadas(Estado, Terminadas, Estado_), marca_ilhas_terminadas(Estado_, Terminadas, Novo_estado).


% junta_pontes: 
% usa a funcao junta_pontes_aux para adicionar as pontes, usando depois os predicados actualiza_vizinhas_apos_pontes e trata_ilhas_terminadas.

% adiciona_ponte usa-se para adicionar as entradas de Ilha1 e Ilha2 a ponte correspondente (Num_pontes vezes).

adiciona_ponte([Ilha, Vizinhas, Pontes], 0, _, _, Entrada) :- Entrada = [Ilha, Vizinhas, Pontes].

adiciona_ponte([Ilha, Vizinhas, Pontes], Num_pontes, ilha(X_1, Pos1), ilha(X_2, Pos2), Entrada) :- 
    cria_ponte(Pos1, Pos2, Ponte), Num_pontes_ is Num_pontes - 1, append(Pontes, [Ponte], Pontes_), 
    adiciona_ponte([Ilha, Vizinhas, Pontes_], Num_pontes_, ilha(X_1, Pos1), ilha(X_2, Pos2), Entrada).


% junta_pontes_aux adiciona ao estado as pontes entre Ilha1 e Ilha2, usando adiciona_ponte caso a ilha da entrada a ser analisada
% seja igual a Ilha1 ou Ilha2

junta_pontes_aux([], _, _, _, Estado, Novo_Estado) :- Novo_Estado = Estado.

junta_pontes_aux([[Ilha, Vizinhas, Pontes]|Resto], Num_pontes, Ilha1, Ilha2, Entradas, Novo_estado) :- 
    Ilha1 = Ilha ->  adiciona_ponte([Ilha, Vizinhas, Pontes], Num_pontes, Ilha1, Ilha2, Entrada), 
    append(Entradas, [Entrada], Entradas_), junta_pontes_aux(Resto, Num_pontes, Ilha1, Ilha2, Entradas_, Novo_estado); 
    Ilha2 = Ilha ->  adiciona_ponte([Ilha, Vizinhas, Pontes], Num_pontes, Ilha1, Ilha2, Entrada), 
    append(Entradas, [Entrada], Entradas_), junta_pontes_aux(Resto, Num_pontes, Ilha1, Ilha2, Entradas_, Novo_estado); 
    append(Entradas, [[Ilha, Vizinhas, Pontes]], Entradas_),
    junta_pontes_aux(Resto, Num_pontes, Ilha1, Ilha2, Entradas_, Novo_estado).


junta_pontes(Estado, Num_pontes, ilha(X_1, Pos1), ilha(X_2, Pos2), Novo_estado) :- 
    junta_pontes_aux(Estado, Num_pontes, ilha(X_1, Pos1), ilha(X_2, Pos2), [], Estado_com_pontes),
    actualiza_vizinhas_apos_pontes(Estado_com_pontes, Pos1, Pos2, Estado_), trata_ilhas_terminadas(Estado_, Novo_estado).

