:- [codigo_comum, puzzles_publicos].

extrai_ilhas_linha(N_L, Linha, Ilhas):-
    findall(ilha(N, (N_L, C)), (member(N, Linha),nth1(C,Linha,N), N>0), Ilhas).		