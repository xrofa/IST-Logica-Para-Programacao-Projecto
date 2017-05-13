% 67381 Christian Sousa
 
% Logica para Programacao
% Projecto
% Solucionador de problemas de Sudoku

% O objectivo deste projecto e escrever um programa em PROLOG para resolver problemas
% de Sudoku. 
% Um problema de Sudoku de dimensao n (em que n e um quadrado perfeito) e uma grelha 
% nxn, em que cada posicao tem um numero entre 1 e n ou esta vazia. 
% Uma grelha de Sudoku encontra-se dividida em linhas, colunas e blocos, todos numerados
% de 1 a n.
% Designa-se por grupo de posicoes, ou simplesmente grupo, o conjunto de posicoes
% correspondentes a uma linha, coluna ou bloco. O objectivo e preencher as posicoes vazias
% com numeros entre 1 e n, de modo a que cada grupo contenha todos os numero entre 1 e n. 

:- include('SUDOKU').
:- include('exemplos').

%-----------------------------------------------------------------------------------------
%
%         Predicado auxiliares para a implementacao dos predicados do projecto
%                                
%-----------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------
% first(Lista, Elemento) significa que Elemento e o primeiro elemento da lista Lista
%-----------------------------------------------------------------------------------------
first([H|_], H).


%-----------------------------------------------------------------------------------------
% remove_primeiro_elemento_lista(Lista, N_Lista) significa que N_Lista e a lista 
% resultante de retirar o primeiro elemento da lista Lista
%-----------------------------------------------------------------------------------------
remove_primeiro_elemento_lista([_|Tail], Tail).	


%-----------------------------------------------------------------------------------------
% unitaria(Lista) significa que a lista Lista tem apenas um elemento
%-----------------------------------------------------------------------------------------
unitaria(Lista) :-
	length(Lista,1),!.


%-----------------------------------------------------------------------------------------
% limpa_lista(Lista, N_Lista) Lista pode conter listas nao unitarias, N_Lista e
% a lista que apenas contem listas unitarias
%-----------------------------------------------------------------------------------------
limpa_lista([],[]). %caso de paragem: lista vazia

limpa_lista([H|T], TT) :-
	msort(H,SH), %ordena a lista
	\+(unitaria(SH)),!, %se o primeiro elemento da lista nao e unitaria
	limpa_lista(T, TT). %chama recursivamente e em TT esta so o resto da lista
						 	
limpa_lista([H|T], [H|TT]) :-	
	msort(H,SH), %ordena a lista
	unitaria(SH),!, %se o primeiro elemento da lista for unitario
	limpa_lista(T, TT). % chama recursivamente e em TT esta o elemento unitario e o resto


%-----------------------------------------------------------------------------------------
% lista_pos_nao_unitarias(Puz, Posicoes) significa que Posicoes sao as posicoes do puzzle
% Puz nas quais o conteudo e nao unitario
%-----------------------------------------------------------------------------------------
lista_pos_nao_unitarias(Puz, Posicoes) :-
	todas_posicoes(Todas_Posicoes), %vai buscar todas as posicoes do Puz
    once(lista_pos_nao_unitarias_aux(Puz,Posicoes,Todas_Posicoes,[])).
    
lista_pos_nao_unitarias_aux(_Puz,Posicoes,[],Posicoes).

lista_pos_nao_unitarias_aux(Puz, Posicoes, [Pos|T], Posicoes_Nao_Unitarias) :-	
	puzzle_ref(Puz,Pos,Cont), %vai buscar o conteudo da posicao
	\+(unitaria(Cont)),!, % verifica que nao e unitaria
    append(Posicoes_Nao_Unitarias, [Pos], Lista_Intermedia), %coloca na lista nao unitaria
    lista_pos_nao_unitarias_aux(Puz, Posicoes, T, Lista_Intermedia).
    																
lista_pos_nao_unitarias_aux(Puz, Posicoes, [Pos|T], Posicoes_Nao_Unitarias) :-	
	puzzle_ref(Puz, Pos, Cont),
    unitaria(Cont),!, % se for unitaria, chama recursivamente mas nao adiciona a lista
    lista_pos_nao_unitarias_aux(Puz,Posicoes,T, Posicoes_Nao_Unitarias).


%-----------------------------------------------------------------------------------------
% pos_nao_unitaria(Puz,Pos) significa que Pos e uma posicao nao unitaria do puzzle Puz
%-----------------------------------------------------------------------------------------
pos_nao_unitaria(Puz,Pos):-		
	lista_pos_nao_unitarias(Puz, Lista), %vai buscar todas as posicoes nao unitarias
	once(member(Pos,Lista)). %Pos e uma posicao nao unitaria
						
								
%-----------------------------------------------------------------------------------------
%
%         Predicado auxiliares para a implementacao dos predicados do projecto
%                                FIM
%-----------------------------------------------------------------------------------------








%-----------------------------------------------------------------------------------------
%
%         				Predicados para a propagacao de mudancas
%
% Propagar a mudanca significa que de cada vez que e colocada uma sequencia unitaria numa
% posicao, o numero dessa sequencia deve ser retirado das sequencias de todas as outras
% posicoes da mesma linha, coluna ou bloco. Se em consequencia alguma sequencia se tornar
% unitaria, esta mudanca devera ser propagada da mesma forma.                                
%-----------------------------------------------------------------------------------------
	
		
%-----------------------------------------------------------------------------------------
% tira_num_aux(Num,Puz,Pos,N_Puz) : N_Puz e o puzzle resultante de
% tirar o numero Num da posicao Pos do puzzle Puz.
%-----------------------------------------------------------------------------------------
tira_num_aux(Num , Puz , Pos , N_Puz) :-	
	puzzle_ref(Puz, Pos , Cont), %vai buscar o conteudo da Pos
	subtract(Cont, [Num], ContASubstituir),%retira o Numero do conteudo
	puzzle_muda_propaga(Puz, Pos, ContASubstituir, N_Puz). %propaga a mudanca
											
											
%-----------------------------------------------------------------------------------------
% tira_num(Num,Puz,Posicoes,N_Puz) : Significa que N_Puz e o puzzle
% resultante de tirar o numero Num de todas as Posicoes do Puzzle
%-----------------------------------------------------------------------------------------
tira_num(Num, Puz, Posicoes, N_Puz) :- 
	percorre_muda_Puz(Puz, tira_num_aux(Num), Posicoes, N_Puz).
	%recorre-se ao predicado fornecido pelos professores para tirar o numero
	%de todas as posicoes										


%-----------------------------------------------------------------------------------------
% puzzle_muda_propaga(Puz,Pos,Cont,N_puz) : faz o mesmo que puzzle_muda 
% mas no caso de Cont ser uma lista unitaria, propaga a mudanca
%-----------------------------------------------------------------------------------------																							
puzzle_muda_propaga(Puz,Pos,Cont,Puz):-		
	puzzle_ref(Puz,Pos,Cont2), %se o conteudo for igual ao que ja existe nessa posicao
	Cont=Cont2,!. %Nao existe mudanca no Puzzle
											
puzzle_muda_propaga(Puz,Pos,Cont,N_Puz):-	
	unitaria(Cont),!, %Se o Cont for unitario
	first(Cont,Num), 
	puzzle_muda(Puz,Pos,[Num],N_Puz2), %muda-se o numero e propaga-se
	posicoes_relacionadas(Pos,Posicoes),
	tira_num(Num,N_Puz2,Posicoes, N_Puz).
												
puzzle_muda_propaga(Puz,Pos,Cont,N_Puz):-	
	puzzle_muda(Puz,Pos,Cont,N_Puz). %se nao for unitario, muda apenas e nao propaga															
	
	
	
	
	
	
																																	
	
%-----------------------------------------------------------------------------------------
%
%         				Predicados para a inicializacao de puzzles
%
% Quando um puzzle e inicializado, as posicoes que se encontravam vazias no puzzle 
% original passam, no puzzle inicializado, a conter sequencias ordenadas e sem elementos
% repetidos dos numeros possiveis. Um numero e possivel para uma posicao se esse numero
% nao ocorrer numa sequencia unitaria na mesma linha, coluna ou bloco. De cada vez que
% e colocada uma sequencia unitaria numa posicao, essa mudanca deve ser propagada ao 
% resto do puzzle.
%-----------------------------------------------------------------------------------------

																												
%-----------------------------------------------------------------------------------------
% possibilidades(Pos,Puz,Poss) significa que Poss e a lista de numeros
% possiveis para a posicao Pos, do puzzle Puz. NOTA: Este predicado  
% apenas deve ser usado para posicoes cujo conteudo nao e uma sequencia
% unitaria.
%-----------------------------------------------------------------------------------------										 												
possibilidades(Pos,Puz,Cont):-			
	puzzle_ref(Puz,Pos,Cont), 
	unitaria(Cont),!. %se o Conteudo da posicao e unitaria nao faz nada
										
possibilidades(Pos,Puz,Poss):-
	puzzle_ref(Puz,Pos,Cont),
	\+(unitaria(Cont)),!, %se nao for unitaria
	numeros(L), %cria a lista dos numeros de 1 a N(dimensao do puzzle)
	posicoes_relacionadas(Pos,Posicoes), %vai buscar as Posicoes relacionadas
	conteudos_posicoes(Puz,Posicoes,Conteudos), %vai buscar os conteudos dessas posicoes
	limpa_lista(Conteudos,Temp), %fica com a lista de conteudos unitarios
	flatten(Temp, PossA), %fica com a lista de numeros em vez de lista de listas
	subtract(L, PossA, Poss). %retira a L os numeros da lista anterior
										
																				
%-----------------------------------------------------------------------------------------
% inicializa_aux(Puz,Pos,N_Puz): significa que N_Puz e o puzzle resultante de colocar
% na posicao Pos do puzzle Puz a lista com os numeros possiveis para essa posicao.
% Note que, se o conteudo da posicao Pos de Puz ja for uma lista unitaria, nada e
% alterado
%-----------------------------------------------------------------------------------------
inicializa_aux(Puz,Pos,Puz):-			
	puzzle_ref(Puz,Pos,Cont),
	unitaria(Cont),!. %se o conteudo da posicao e unitaria nao faz nada
									
inicializa_aux(Puz,Pos,N_Puz):-			
	puzzle_ref(Puz,Pos,Cont),
	length(Cont,0),!, %se a posicao estiver vazia
	possibilidades(Pos,Puz,Poss), %calcula as possibilidades para essa posicao
	puzzle_muda_propaga(Puz,Pos,Poss,N_Puz). %muda e propaga 
										
inicializa_aux(Puz,Pos,N_Puz):-			
	puzzle_ref(Puz,Pos,Cont),
	\+(unitaria(Cont)),!, %se a posicao nao for unitaria
	possibilidades(Pos,Puz,Poss), %calcula as possibilidades para essa posicao
	puzzle_muda_propaga(Puz,Pos,Poss,N_Puz).%propaga as mudancas
								
										
%-----------------------------------------------------------------------------------------
% inicializa(Puz,N_Puz): Significa que N_Puz e o puzzle resultante de inicializar o puzzle
% Puz
%-----------------------------------------------------------------------------------------
inicializa(Puz,N_Puz) :-
	todas_posicoes(Todas_Posicoes), %vai buscar todas as posicoes do puzzle
	percorre_muda_Puz(Puz,inicializa_aux,Todas_Posicoes,N_Puz).
	%recorre-se ao predicado fornecido pelos professores para inicializar
	%cada uma das posicoes nao unitarias do puzzle Puz
									
		
		
		
		
			
									
%-----------------------------------------------------------------------------------------
%
%         				Predicados para a inspeccao de puzzles
%
% Apos o puzzle inicializado, as suas linhas, colunas e blocos devem ser percorridos
% verificando se existe algum numero que apenas ocorre numa das posicoes de uma linha
% coluna ou bloco. Se tal acontecer, a sequencia correspondente deve passar a ter apenas
% esse numero, e esta mudanca deve ser propagada, tal como durante a inicializacao
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
% so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num): significa que o numero Num so aparece numa
% das posicoes da lista Posicoes do puzzle Puz, e que essa posicao e Pos_Num.
%-----------------------------------------------------------------------------------------
so_aparece_uma_vez(Puz, Num, [Pos_Num], Pos_Num) :-		
	puzzle_ref(Puz, Pos_Num, Cont),!,
   	member(Num, Cont).%verifica se o Num existe no Conteudo da Posicao Pos
   														
so_aparece_uma_vez(Puz, Num, [Pos_Num, Next_Pos|T], Pos_Num) :-		
	puzzle_ref(Puz, Pos_Num, Cont_Pos_Num),
    member(Num, Cont_Pos_Num),!,
    puzzle_ref(Puz, Next_Pos, Cont_Next_Pos),
    \+(member(Num, Cont_Next_Pos)),!, %se nao for membro da posicao seguinte
    so_aparece_uma_vez(Puz, Num, [Pos_Num|T], Pos_Num). %recursiva com a posicao e o resto
    																
so_aparece_uma_vez(Puz, Num, [H|T], Pos_Num) :-		
	puzzle_ref(Puz, H, Cont),
    \+(member(Num, Cont)),!, %se for membro da posicao
    so_aparece_uma_vez(Puz, Num, T, Pos_Num). %recursiva apenas com o resto da lista


%-----------------------------------------------------------------------------------------
% inspecciona_num(Posicoes,Puz,Num,N_Puz) significa que N_Puz e o resultado de inspeccionar
% o grupo cujas posicoes sao Posicoes, para o numero Num:
% - se Num so ocorrer numas das posicoes de Posicoes e se o conteudo dessa posicao nao for 
% uma lista unitaria, esse conteudo e mudado para [Num] e esta mudanca e propagada.
% - Caso contrario, Puz=N_Puz.
%-----------------------------------------------------------------------------------------
inspecciona_num(Posicoes,Puz,Num,Puz):-		
	so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num), %se o numero aparece so uma vez
	puzzle_ref(Puz,Pos_Num,Cont),
	unitaria(Cont),!. %se for unitario nao faz nada

inspecciona_num(Posicoes,Puz,Num,N_Puz):-	
	so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num), %se o numero aparece so uma vez
	puzzle_ref(Puz, Pos_Num, Cont), 
	\+(unitaria(Cont)),!, %se o conteudo nao for unitario
	puzzle_muda_propaga(Puz,Pos_Num,[Num],N_Puz). %muda e propaga
											
inspecciona_num(Posicoes,Puz,Num,Puz):-		
	\+(so_aparece_uma_vez(Puz,Num,Posicoes,_Pos_Num)).
	%se nao aparece so uma vez, entao o Puz resultante e igual ao inicial


%-----------------------------------------------------------------------------------------
% inspecciona_grupo(Puz,Gr,N_Puz) inspecciona o grupo cujas posicoes sao as da lista Gr,
% do puzzle Puz para cada um dos numeros possiveis, sendo o resultado o puzzle N_Puz.
%-----------------------------------------------------------------------------------------										
inspecciona_grupo(Puz,Gr,N_Puz):-		
	numeros(L), %cria a lista com os valores de 1 a N(dimensao do puzzle)
	inspecciona_grupo_aux(Puz, Gr, N_Puz, L). 
										
inspecciona_grupo_aux(Puz,_Gr, Puz, []).

inspecciona_grupo_aux(Puz,Gr,N_Puz, [H|T]):-	
	inspecciona_num(Gr,Puz,H,Puz_Temp), %inspecciona o numero para a primeira posicao
	inspecciona_grupo_aux(Puz_Temp,Gr,N_Puz,T).
	%chamada recursiva para inspeccionar as restantes posicoes


%-----------------------------------------------------------------------------------------
% inspecciona(Puz,N_Puz) inspecciona cada um dos grupos do puzzle Puz, para casa um dos 
% numeros possiveis, sendo o resultado o puzzle N_Puz. 
%-----------------------------------------------------------------------------------------
inspecciona(Puz, N_Puz):-	
	grupos(Gr), %cria a lista de grupos do Puzzle
	percorre_muda_Puz(Puz, inspecciona_grupo,Gr, N_Puz).
	%recorre-se ao predicado fornecido pelos professores para inspeccionar
	%cada um dos grupos do Puzzle
		
							
		
		
		
		
		
		
%-----------------------------------------------------------------------------------------
%
%         				Predicados para a verificacao de solucoes
%
% Apos a inspeccao do puzzle, podera existir uma solucao para o puzzle dado, os predicados
% seguintes verificam a validade da solucao, isto e, cada linha, coluna e bloco contem
% todos os numeros de 1 a N(dimensao do puzzle).
%-----------------------------------------------------------------------------------------

					
%-----------------------------------------------------------------------------------------
% grupo_correcto(Puz,Nums,Gr), em que Puz e um puzzle, significa que o grupo de Puz cujas
% posicoes sao as da lista Gr esta correcto, isto e, que contem todos os numeros da lista
% Nums, sem repeticoes.
%-----------------------------------------------------------------------------------------
grupo_correcto(Puz,Nums,Gr):-	
	conteudos_posicoes(Puz, Gr, Conteudos), %vai buscar o conteudo das posicoes dadas
	msort(Conteudos, Conteudos_Ordenados), %ordena os conteudos sem a remocao de dups.
	flatten(Conteudos_Ordenados, Conteudos_Teste), %cria uma lista com valores apenas
	%em vez de lista de listas
	Nums == Conteudos_Teste. %verifica se os conteudos sao os que constam na lista Nums
											
												
%-----------------------------------------------------------------------------------------
% solucao(Puz) significa que o puzzle Puz e uma solucao, isto e, que todos os seus grupos 
% contem todos os numeros possiveis, sem repeticoes
%-----------------------------------------------------------------------------------------
solucao(Puz):-					
	grupos(Gr), %cria a lista de grupos do Puzzle
	numeros(L), %cria a lista de numeros de 1 a N(dimensao do puzzle)
	maplist(grupo_correcto(Puz,L), Gr). 
	%faz a verificacao se cada grupo Gr do Puzzle Puz tem apenas os numeros de 1 a N
								
	
	
	
	
	
	
	
%-----------------------------------------------------------------------------------------
%
%         				Predicados Resolve
%
% Predicado principal do projecto, que dado um puzzle, nos permite obter a solucao, se
% a mesma existir.
%-----------------------------------------------------------------------------------------

															
%-----------------------------------------------------------------------------------------
% resolve(Puz,Sol) significa que o puzzle Sol e uma/a solucao do puzzle Puz. Na obtencao
% da solucao, deve ser utilizado o algoritmo apresentado na Seccao 1: Inicializar o puzzle
% inspeccionar as linhas, colunas e blocos, e so entao procurar uma solucao, tal como 
% descrito na Seccao 1.4
%-----------------------------------------------------------------------------------------
resolve(Puz,Sol):-			
	inicializa(Puz, Puz_Inicializado), %inicializa o puzzle
	inspecciona(Puz_Inicializado, Puz_Inspeccionado), %inspecciona o puzzle
	resolve_aux(Puz_Inspeccionado, Sol),!. %comeca a tentar encontrar uma solucao 
							
resolve_aux(Puz,Puz):-	%Caso de Paragem:	
	solucao(Puz). %Quando o puzzle recebido e uma solucao

resolve_aux(Puz,Sol):-  %Caso Geral:
	% escolhe uma posicao do puzzle que tem um conteudo nao unitario, Pos. 
	% Este conteudo nao pode ser uma lista vazia.
	once(pos_nao_unitaria(Puz, Pos)),
	puzzle_ref(Puz, Pos, Cont),
	\+(length(Cont,0)),
	% escolhe um numero desse conteudo, Num
	member(Num,Cont),
	% mudar com o propagacao o conteudo da Pos para [Num]
	puzzle_muda_propaga(Puz,Pos,[Num],N_Puz),
	%repetir com o novo puzzle ate ser encontrada uma solucao
	resolve_aux(N_Puz,Sol).