%--------------------------------------------------------------------------------------------------------------------------
/*extrai_ilhas_linha(N_L,Linha,Ilhas), em que N_L e um inteiro positivo correspondente ao numero
 de uma linha e Linha e uma lista correspondente a linha de um puzzle, significa que Ilhas e a
 lista ordenada(da esqueda para a direita) cujos elementos sao as ilhas de Linha*/
%--------------------------------------------------------------------------------------------------------------------------

extrai_ilhas_linha(N_L, Linha, Ilhas):- extrai_ilhas_linha(N_L, Linha, 1, Ilhas).

extrai_ilhas_linha(_,[],_,[]).
extrai_ilhas_linha(N_L, [H|T], Contador, [ilha(H,(N_L,Contador))|Res]):-  %Caso1: Insere ilha em Ilhas se H > 0,
    Contador1 is Contador + 1,                                            %       Incrementa contador
 	H > 0,
    extrai_ilhas_linha(N_L, T, Contador1, Res).

extrai_ilhas_linha(N_L, [H|T], Contador, Res):-                           %Caso2: Nao insere ilha em Ilhas,
    Contador1 is Contador + 1,                                            %       Incrementa contador
 	H == 0,
    extrai_ilhas_linha(N_L, T, Contador1, Res).

%--------------------------------------------------------------------------------------------------------------------------
/*ilhas(Puz,ilhas), em que Puz e um puzzle, significa que Ilhas e a lista 
 ordenada(esquerda para a direita e cima para baixo) cujos elementos sao as ilhas de Puz*/
%--------------------------------------------------------------------------------------------------------------------------

ilhas(Puz, Ilhas):- ilhas(Puz, 1, Ilhas1), append(Ilhas1, Ilhas).

ilhas([], _, []).
ilhas([H|T], Contador, [Ilhas|Res]):-                          
	Contador1 is Contador + 1,                     
    extrai_ilhas_linha(Contador, H, Ilhas),       % Extrai as ilhas de cada linha
    ilhas(T, Contador1, Res).

%--------------------------------------------------------------------------------------------------------------------------
/*vizinhas(Ilhas,Ilha,Vizinhas), em que Ilhas e a lista de ilhas de um puzzle e Ilhas e uma dessas ilhas, significa que Vizinhas
 e a lista ordenada(cima para baixo e esquerda para a direita) cujos elementos sao as ilhas vizinhas de Ilha
vizinhas usa vizinhasCima(Ilhas,Ilha1,Res), vizinhasEsquerda(Ilhas,Ilha1,Res), vizinhasDireita(Ilhas,Ilha1,Res), 
 vizinhasBaixo(Ilhas,Ilha1,Res) para encontrar as ilhas na mesma linha e coluna, que depois atraves de menorCima(Ilhas,Min),
 menorEsquerda(Ilhas,Min), menorDireita(Ilhas,Min), menorBaixo(Ilhas,Min) e feita uma selecao da ilha que esta mais proxima
 e e considerada a vizinha.
juntaRes(Ilhas,Res) aloja as ilhas vizinhas em Res*/
%--------------------------------------------------------------------------------------------------------------------------

vizinhas(Ilhas,ilha(N,(L,C)),Res):-                        %vizinhas procura as ilhas vizinhas nas quatro direcoes, escolhe
    vizinhasCima(Ilhas,ilha(N,(L,C)),Res11),               % qual a mais proxima e junta tudo para formar o resultado
    menorCima(Res11,Res1),
    vizinhasEsquerda(Ilhas,ilha(N,(L,C)),Res22),
    menorEsquerda(Res22,Res2),
    vizinhasDireita(Ilhas,ilha(N,(L,C)),Res33),
    menorDireita(Res33,Res3),
    vizinhasBaixo(Ilhas,ilha(N,(L,C)),Res44),
    menorBaixo(Res44,Res4),
    juntaRes([Res1,Res2,Res3,Res4], Res).

juntaRes([],[]).                                           %juntaRes remove listas vazias caso haja direcoes sem ilhas
juntaRes([H|T],[H|Res]):-
    H \== [],
    juntaRes(T,Res).
juntaRes([H|T], Res):-
    H == [],
    juntaRes(T,Res).


vizinhasCima(Ilhas,ilha(_,(L,C)),Res):-                                                  %vizinhas_Cima_Baixo_Esquerda_Direita
    findall(ilha(N1,(L1,C1)), (member(ilha(N1,(L1,C1)), Ilhas),L1<L,C1==C),Res).         % procura todas as ilhas na mesma coluna
vizinhasBaixo(Ilhas,ilha(_,(L,C)),Res):-                                                 % ou linha na direcao correspondente
    findall(ilha(N1,(L1,C1)), (member(ilha(N1,(L1,C1)), Ilhas),L1>L,C1==C),Res).
vizinhasEsquerda(Ilhas,ilha(_,(L,C)),Res):-
    findall(ilha(N1,(L1,C1)), (member(ilha(N1,(L1,C1)), Ilhas),L1==L,C1<C),Res).
vizinhasDireita(Ilhas,ilha(_,(L,C)),Res):-
    findall(ilha(N1,(L1,C1)), (member(ilha(N1,(L1,C1)), Ilhas),L1==L,C1>C),Res).


menorCima([],[]).                                             %menor_Cima_Baixo_Esquerda_Direita decide qual das ilhas a mais
menorCima([H|T],Min):- menorCima(T,H,Min).                    % proxima da ilha inicial
menorCima([],X,X).
menorCima([ilha(N,(L,C))|T],ilha(_,(L1,_)),Minimo):-
    L>L1,
    menorCima(T,ilha(N,(L,C)),Minimo).
menorCima([ilha(_,(L,_))|T],ilha(N1,(L1,C1)),Minimo):-
    L<L1,
    menorCima(T,ilha(N1,(L1,C1)),Minimo).

menorBaixo([],[]).
menorBaixo([H|T],Min):- menorBaixo(T,H,Min).
menorBaixo([],X,X).
menorBaixo([ilha(N,(L,C))|T],ilha(_,(L1,_)),Minimo):-
    L<L1,
    menorBaixo(T,ilha(N,(L,C)),Minimo).
menorBaixo([ilha(_,(L,_))|T],ilha(N1,(L1,C1)),Minimo):-
    L>L1,
    menorBaixo(T,ilha(N1,(L1,C1)),Minimo).

menorEsquerda([],[]).
menorEsquerda([H|T],Min):- menorEsquerda(T,H,Min).
menorEsquerda([],X,X).
menorEsquerda([ilha(N,(L,C))|T],ilha(_,(_,C1)),Minimo):-
    C>C1,
    menorEsquerda(T,ilha(N,(L,C)),Minimo).
menorEsquerda([ilha(_,(_,C))|T],ilha(N1,(L1,C1)),Minimo):-
    C<C1,
    menorEsquerda(T,ilha(N1,(L1,C1)),Minimo).

menorDireita([],[]).
menorDireita([H|T],Min):- menorDireita(T,H,Min).
menorDireita([],X,X).
menorDireita([ilha(N,(L,C))|T],ilha(_,(_,C1)),Minimo):-
    C<C1,
    menorDireita(T,ilha(N,(L,C)),Minimo).
menorDireita([ilha(_,(_,C))|T],ilha(N1,(L1,C1)),Minimo):-
    C>C1,
    menorDireita(T,ilha(N1,(L1,C1)),Minimo).

%--------------------------------------------------------------------------------------------------------------------------
/*estado(Ilhas,Estado), em que Ilhas e a lista de ilhas de um puzzle, significa que Estado e a lista ordenada cujos elementos 
 sao as entradas referentes a cada uma das ilhas de Ilhas*/
%--------------------------------------------------------------------------------------------------------------------------

estado([],[]). 
estado(Ilhas,Estado):-                                 %estado gera todas as entradas das ilhas do puzzle
    findall([ilha(N,(L,C)), Vizinhas, []], 
            (member(ilha(N,(L,C)),Ilhas), vizinhas(Ilhas,ilha(N,(L,C)),Vizinhas)), 
            Estado).

%--------------------------------------------------------------------------------------------------------------------------
/*posicoes_entre(Pos1,Pos2,Posicoes), em que Pos1 e Pos2 sao Posicoes, significa que Posicoes  e a lista ordenada de posicoes 
 entre Pos1 e Pos2(exclusive). Se Pos1 e Pos2 nao pertencerem a mesma ilha ou a mesma coluna, o resultado e false*/
%--------------------------------------------------------------------------------------------------------------------------

posicoes_entre(X,X,[X]).

posicoes_entre((L1,C1), (L2,C2), []):-         
    C1 == C2,
    Z is L1 + 1,
    Z == L2.

posicoes_entre((L1,C1), (L2,C2), []):-
    C1 == C2,
    Z is L2 + 1,
    Z == L1.

posicoes_entre((L1,C1), (L2,C2), []):-                  %posicoes_entre navega uma das quatro orientacoes possiveis, escolhida
    L1 == L2,                                           % pelos criterios da regra, e aumenta o valor da linha ou da coluna em +1
    Z is C1 + 1,                                        % para gerar todas as posicoes entre as duas ilhas inicias, ate chegar a
    Z == C2.                                            % ilha de destino

posicoes_entre((L1,C1), (L2,C2), []):-                  
    L1 == L2,
    Z is C2 + 1,
    Z == C1.

posicoes_entre((L1,C1), (L2,C2), [(Z,C1)|Res]):-        %Orientacao: mesma coluna, para baixo
    C1 == C2, 
    L1 < L2,  
    Z is L1 + 1,
    Z \== L2,
    !,
    posicoes_entre((Z,C1), (L2,C2), Res).

posicoes_entre((L1,C1), (L2,C2), [(Z,C1)|Res]):-        %Orientacao: mesma coluna, para cima
    C1 == C2,
    L1 > L2,
    Z is L2 + 1,
    Z \== L1,
    !,
    posicoes_entre((L1,C1), (Z,C2), Res).

posicoes_entre((L1,C1), (L2,C2), [(L1,Z)|Res]):-        %Orientacao: mesma linha, para a direita
    L1 == L2,
    C1 < C2,
    Z is C1 + 1,
    Z \== C2,
    !,
    posicoes_entre((L1,Z), (L2,C2), Res).

posicoes_entre((L1,C1), (L2,C2), [(L1,Z)|Res]):-        %Orientacao: mesma linha, para a esquerda
    L1 == L2,
    C1 > C2,
    Z is C2 + 1,
    Z \== C1,
    !,
    posicoes_entre((L1,C1), (L2,Z), Res).

%--------------------------------------------------------------------------------------------------------------------------
/*cria_ponte(Pos1,Pos2,Ponte), em que Pos1 e Pos2 sao 2 posicoes, significa que Ponte e uma ponte entre essas duas posicoes*/
%--------------------------------------------------------------------------------------------------------------------------

cria_ponte((L1,C1), (L2,C2), ponte((L1,C1),(L2,C2))):-            
    L1 < L2; C1 < C2.
cria_ponte((L1,C1), (L2,C2), ponte((L2,C2),(L1,C1))):-
    L1 > L2; C1 > C2.

%--------------------------------------------------------------------------------------------------------------------------
/*caminho_livre(Pos1,Pos2,Posicoes,I,Vz), em que Pos1 e Pos2 sao as posicoes, Posicoes e a lista de posicoes entre Pos1 e 
 Pos2, I e uma ilha e Vz e uma das suas vizinhas, significa que a adicao da ponte ponte(Pos1,Pos2) nao faz com que I e Vz
 deixem de ser vizinhas*/
%--------------------------------------------------------------------------------------------------------------------------



caminho_livre((L1,C1), (L2,C2), Ilhas_Entre, ilha(_,(L,C)), ilha(_,(Lv,Cv))):-   %caminho_livre vai procurar as posicoes entre
    posicoes_entre((L1,C1), (L2,C2), Ilhas_Entre),                               % duas posicoes dadas e procura tambem as posicoes 
    (L==Lv;C==Cv),                                                               % entre a ilha e a vizinha, verificando se ha
    posicoes_entre((L,C),(Lv,Cv),Ilhas_Entre_v),                                 % uma interseccao entre as duas listas. Se sim,
    interseccao(Ilhas_Entre,Ilhas_Entre_v, []).                                  % ilha e vizinha deixam de ser vizinhas

caminho_livre((L1,C1), (L2,C2), Ilhas_Entre, ilha(_,(L,C)), ilha(_,(Lv,Cv))):-
    posicoes_entre((L1,C1), (L2,C2), Ilhas_Entre),
    (L==Lv;C==Cv),
    L==L1,Lv==L2,C==C1,Cv==C2.
caminho_livre((L1,C1), (L2,C2), Ilhas_Entre, ilha(_,(L,C)), ilha(_,(Lv,Cv))):-
    posicoes_entre((L1,C1),(L2,C2), Ilhas_Entre),
    (L==Lv;C==Cv),
    L==L2,Lv==L1,C==C2,Cv==C1.

interseccao([], _, []).
interseccao([H|L1T], L2, L3) :-                     
        memberchk(H, L2),
        !,
        L3 = [H|L3T],
        interseccao(L1T, L2, L3T).
interseccao([_|L1T], L2, L3) :-
        interseccao(L1T, L2, L3).

%--------------------------------------------------------------------------------------------------------------------------
/*atualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,Entrada,Nova_entrada) em que Pos1 e Pos2 sao as posicoes entre as quais se vizinhasBaixo
 adicionar a ponte e Posicoes as posicoes entre Pos1 e Pos2. Entrada e uma entrada e Nova_Entrada e igual a Entrada mas as ilhas
 vizinhas serao alteradas consoante a adicao da nova ponte*/
%--------------------------------------------------------------------------------------------------------------------------

actualiza_vizinhas_entrada((L1,C1),(L2,C2), Posicoes, [ilha(N,(L,C)), Viz,Pontes], [ilha(N,(L,C)), New_Viz,Pontes]):-
    posicoes_entre((L1,C1),(L2,C2),Posicoes),
    verifica_vizinhos((L1,C1),(L2,C2),Posicoes,ilha(N,(L,C)),Viz,New_Viz).        %actualiza_vizinhas_entrada gera as posicoes entre
                                                                                  % as posicoes dadas e atraves de verifica_vizinhos()
                                                                                  % tera os vizinhos de cada ilha atualizados com a
verifica_vizinhos(_,_,_,_,[],[]).                                                 % adicao da nova ponte
verifica_vizinhos((L1,C1), (L2,C2), Posicoes, ilha(N,(L,C)), [H|T], [H|Res]):-    %Caso1: quanto uma ilha ainda e considerada vizinha
    caminho_livre((L1,C1),(L2,C2), Posicoes, ilha(N,(L,C)), H),!, 
    verifica_vizinhos((L1,C1), (L2,C2), Posicoes, ilha(N,(L,C)), T, Res).
verifica_vizinhos((L1,C1), (L2,C2), Posicoes, ilha(N,(L,C)), [_|T], Res):-        %Caso2: quando uma ilha deixa de ser considerada
    verifica_vizinhos((L1,C1), (L2,C2), Posicoes, ilha(N,(L,C)), T, Res).         % vizinha apos a dicao da ponte

%--------------------------------------------------------------------------------------------------------------------------
/*actualiza_vizinhas_apos_ponte(Estado,Pos1,Pos2,Novo_Estado) em que Estado e o estado do puzzle, Pos1 e Pos2 sao as posicoes entre as
 quais se vai criar uma ponte e Novo_Estado e o estado que se obtem com as novas vizinhancas*/
%--------------------------------------------------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes([],_,_,[]).                           %actualiza_vizinhas_apos_pontes actualiza as vizinhas de
actualiza_vizinhas_apos_pontes([H|T],(L1,C1),(L2,C2),[X|Tr]):-       % toda sas entradas, fazendo poroveito de
    posicoes_entre((L1,C1),(L2,C2),Posicoes),                        % actualiza_vizinhas_entrada()
    actualiza_vizinhas_entrada((L1,C1),(L2,C2),Posicoes,H,X),
    actualiza_vizinhas_apos_pontes(T,(L1,C1),(L2,C2),Tr).

%--------------------------------------------------------------------------------------------------------------------------
/*ilhas_terminadas(Estado,Ilhas_Term) onde Estado e um estado e Ilhas_Term e uma lista com ilhas que ja tem todas as pontes
 associadas, chamadas ilhas terminadas. Se a entrada referente a uma ilha for [ilhas(N,(L,C)), Viz, Pontes], esta ilha ilha
 estara terminada se N for diferente de 'X' e o comprimento da lista Pontes for N*/
%--------------------------------------------------------------------------------------------------------------------------

ilhas_terminadas(Estado,Ilhas_Term):-
    findall(ilha(N,(L,C)),(member([ilha(N,(L,C)),_,Pontes],Estado), integer(N),length(Pontes,N)),Ilhas_Term).

            %ilhas_terminadas percorre todas as entradas e corta as ilhas que tem um X no
            % numero de pontes ou o tamanho de Pontes e igual a N, utilizando o findall
  

%--------------------------------------------------------------------------------------------------------------------------
/*tira_ilhas_terminadas_entrada(Ilhas_Term,Entrada,Nova_Entrada) em que Ilhas_Term e uma lista de ilhas terminadas e Entrada 
 e uma entrada, sendo Nova_Entrada a resultante de remover as ilhas de Ilhas_Term da lista de ilhas vizinhas de Entrada*/
%--------------------------------------------------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_Term,[ilha(N,(L,C)),Vizinhas,Ponte],[ilha(N,(L,C)),NewVizinhas,Ponte]):-
    findall(X,(member(X,Vizinhas),nao_pertence(X,Ilhas_Term)),NewVizinhas).
    
nao_pertence(_, []).              %tira_ilhas_terminadas_entrada remove as ilhas terminadas de uma Entrada, actualizando-a.
nao_pertence(E, [P | R]) :-       
	E \== P,
	nao_pertence(E, R).

%--------------------------------------------------------------------------------------------------------------------------
/*tira_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado) em que Estado e o estado do Puzzle e Ilhas_Term e a lista de ilhas
 terminadas, significa que Novo_Estado e o estado resultante de aplicar tira_ilhas_terminadas_entrada() a cada um
 dos estados*/
%--------------------------------------------------------------------------------------------------------------------------

tira_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado):-      %tira_ilhas_terminadas remove as ilhas terminadas de todas as
    findall(X,                                              % entradas do estado, usando tira_ilhas_terminadas_entrada() para
            (member([ilha(N,(L,C)),Viz,Pontes], Estado),    % cada membro do estado
                
                tira_ilhas_terminadas_entrada(Ilhas_Term,[ilha(N,(L,C)),Viz,Pontes],X)),
            Novo_Estado).

%--------------------------------------------------------------------------------------------------------------------------
/*marca_ilhas_terminadas_entrada(Ilhas_Term,Entrada,Nova_Entrada), em que Ilhas_Term e a lista de ilhas terminadas e Entrada e 
 uma entrada, significa que Nova_Entrada e a entrada obtida de Entrada da seguinte forma: se a ilha de Entrada pertencer a 
 Ilhas_Term, o numero de pontes deste e substituido por 'X', em caso contrario Nova_Entrada e igual a Entrada*/
%--------------------------------------------------------------------------------------------------------------------------

marca_ilhas_terminadas_entrada(Ilhas_Term,[ilha(N,(L,C)),Viz,Pontes],[ilha('X',(L,C)),Viz,Pontes]):-
    member(ilha(N,(L,C)),Ilhas_Term).

marca_ilhas_terminadas_entrada(Ilhas_Term,[ilha(N,(L,C)),Viz,Pontes],[ilha(N,(L,C)),Viz,Pontes]):-
    nao_pertence(ilha(N,(L,C)),Ilhas_Term).

%marca_ilhas_terminadas_entrada usa member() e nao_pertence() para verificar se a ilha de Entrada pertence a Ilhas_terminads. Se
% se pertencer, o numero de pontes passa a X

%--------------------------------------------------------------------------------------------------------------------------
/*marca_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado), em que Estado e o estado do Puzzle e Ilhas_Term e a lista de ilhas
 terminadas, significa que Novo_Estado  e o estado resultante de aplicar o predicado marca_ilhas_terminadas_entrada() a cada
 uma das entradas de Estado*/
%--------------------------------------------------------------------------------------------------------------------------

marca_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado):-    
    findall(Nova_Entrada,
            (member(Entrada,Estado),marca_ilhas_terminadas_entrada(Ilhas_Term,Entrada,Nova_Entrada)),
            Novo_Estado).

%marca_ilhas_terminadas aplica marca_ilhas_terminadas_entrada() a todas as ilhas do Estado

%--------------------------------------------------------------------------------------------------------------------------
/*trata_ilhas_terminadas(Estado,Novo_Estado), em que Estado e o estado do Puzzle e o Novo_Estado e o estado resultante de 
 aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado*/
%--------------------------------------------------------------------------------------------------------------------------

trata_ilhas_terminadas(Estado,Novo_Estado):-                        
    ilhas_terminadas(Estado,Ilhas_Term),
    tira_ilhas_terminadas(Estado,Ilhas_Term,Novo_Estado1),
    marca_ilhas_terminadas(Novo_Estado1,Ilhas_Term,Novo_Estado).

%--------------------------------------------------------------------------------------------------------------------------
/*junta_pontes(Estado,Num_Pontes,Ilha1,Ilha2,Novo_Estado), em que Estado e o estado do Puzzle e Ilha1 e Ilha2 sao duas ilhas,
 significa que Novo_Estado e o estado que se obtem de Estado por adicao de Num_Pontes pontes entre Ilha1 e Ilha2.*/
%--------------------------------------------------------------------------------------------------------------------------

junta_pontes(Estado,Num_Pontes,ilha(_,(L1,C1)),ilha(_,(L2,C2)),Novo_Estado):-  %junta_pontes age como main, chamando as
    posicoes_entre((L1,C1),(L2,C2),Posicoes_Entre),                            % regras necessarias
    adicionar_pontes_entre(Estado,Num_Pontes,Posicoes_Entre,Estado1), 
    cria_ponte((L1,C1),(L2,C2), Ponte),
    adicionar_pontes(Estado1,Num_Pontes,Ponte,Estado2),
    actualiza_vizinhas_apos_pontes(Estado2,(L1,C1),(L2,C2),Estado3),
    trata_ilhas_terminadas(Estado3,Novo_Estado).
    
%adicioanar_pontes_entre aplica adicionar_pontes_vizinhas() a todas as Entradas de Estado
    
adicionar_pontes_entre([],_,_,[]).
adicionar_pontes_entre([[ilha(N,(L,C)),Viz,Pontes]|T],Num_Pontes,Posicoes_Entre,[[ilha(N,(L,C)),Novo_Viz,Pontes]|Res]):-
	adicionar_pontes_vizinhas(Viz,Num_Pontes,Posicoes_Entre,Novo_Viz),
    adicionar_pontes_entre(T,Num_Pontes,Posicoes_Entre,Res).                 

%adicionar_pontes_vizinhas aumenta o numero de pontes de ilhas se estas pertencerem a posicoes entre as ilhas inicias

adicionar_pontes_vizinhas([],_,_,[]).
adicionar_pontes_vizinhas([ilha(N,(L,C))|T],Num_Pontes,Posicoes_Entre,[ilha(N1,(L,C))|Res]):-
    member((L,C),Posicoes_Entre),
    N1 is N+Num_Pontes,
    adicionar_pontes_vizinhas(T,Num_Pontes,Posicoes_Entre,Res).
adicionar_pontes_vizinhas([ilha(N,(L,C))|T],Num_Pontes,Posicoes_Entre,[ilha(N,(L,C))|Res]):-
    nao_pertence((L,C),Posicoes_Entre),
    adicionar_pontes_vizinhas(T,Num_Pontes,Posicoes_Entre,Res).
    
%adicionar_pontes adiciona as pontes entre as duas posicoes iniciais, tendo em conta a as quatro orientacoes possiveis

adicionar_pontes([],_,_,[]).
adicionar_pontes([[ilha(N,(L,C)),Viz,Ponte]|T],Num_Pontes,ponte((L1,C1),(L2,C2)),[[ilha(N,(L,C)),Viz,Nova_Ponte]|Res]):-
    L1==L, C1==C,
    Num_Pontes == 1,
    append(Ponte,[ponte((L1,C1),(L2,C2))], Nova_Ponte),
    adicionar_pontes(T,Num_Pontes,ponte((L1,C1),(L2,C2)),Res).
adicionar_pontes([[ilha(N,(L,C)),Viz,Ponte]|T],Num_Pontes,ponte((L1,C1),(L2,C2)),[[ilha(N,(L,C)),Viz,Nova_Ponte]|Res]):-
    L2==L, C2==C,
    Num_Pontes == 1,
    append(Ponte,[ponte((L1,C1),(L2,C2))], Nova_Ponte),
    adicionar_pontes(T,Num_Pontes,ponte((L1,C1),(L2,C2)),Res).
adicionar_pontes([[ilha(N,(L,C)),Viz,Ponte]|T],Num_Pontes,ponte((L1,C1),(L2,C2)),[[ilha(N,(L,C)),Viz,Nova_Ponte]|Res]):-
    L1==L, C1==C,
    Num_Pontes == 2,
    append(Ponte,[ponte((L1,C1),(L2,C2)),ponte((L1,C1),(L2,C2))], Nova_Ponte),
    adicionar_pontes(T,Num_Pontes,ponte((L1,C1),(L2,C2)),Res).
adicionar_pontes([[ilha(N,(L,C)),Viz,Ponte]|T],Num_Pontes,ponte((L1,C1),(L2,C2)),[[ilha(N,(L,C)),Viz,Nova_Ponte]|Res]):-
    L2==L, C2==C,
    Num_Pontes == 2,
    append(Ponte,[ponte((L1,C1),(L2,C2)),ponte((L1,C1),(L2,C2))], Nova_Ponte),
    adicionar_pontes(T,Num_Pontes,ponte((L1,C1),(L2,C2)),Res).
adicionar_pontes([[ilha(N,(L,C)),Viz,Ponte]|T],Num_Pontes,ponte((L1,C1),(L2,C2)),[[ilha(N,(L,C)),Viz,Ponte]|Res]):-
	not((L1==L,C1==C);(L2==L,C2==C)),
    adicionar_pontes(T,Num_Pontes,ponte((L1,C1),(L2,C2)),Res).
