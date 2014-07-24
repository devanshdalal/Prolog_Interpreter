%%%%%%%%%%%%%%%%%%      Family Tree of Mahabharat     %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%      female characters   		  %%%%%%%%%%%%%%%%%%%%%%%%%%%
female(kunti).
female(madri).
female(hidimba).
female(satyavati).
female(ganga).
female(chitranagda).
female(ambika).
female(ambalika).
female(gandhari).
female(dushala).
female(sukhada).
female(draupadi).
female(subhadra).
female(rohini).
female(devaki).
female(uttara).
female(kripi).
female(shikandi).
female(sudarma).
female(agni).
female(janpadi).
female(bhanumati).
female(parishrami).

%%%%%%%%%%%%%%%%%%       male characters     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
male(pandu).
male(subala).
male(shakuni).
male(suryadeva).
male(shantanu).
male(vichitravirya).
male(vyasa).
male(vidur).
male(drupada).
male(dhritrashtra).
male(dhrishtadyumma).
male(shikandi).
male(parashara).
male(bheeshma).

male(yudhisthir).
male(arjun).
male(nakul).
male(bheema).
male(sahdev).
male(karana).
male(ghatotkatch).
male(duryodhan).
male(dushasn).
male(jayadratha).
male(yuyutsu).
male(vikrana).
male(abhimanyu).
male(vasudeva).
male(balram).
male(krishna).
male(virata).
male(prikshit).
male(sharadwan).
male(kripacharya).
male(dronacharya).
male(aswathama).
male(lakshmana).
male(ulook).

%%%%%%%%%%%%%%%   married relation ship    %%%%%%%%%%%%%

married(kunti,pandu).
married(madri,pandu).
married(hidimba,bheema).
married(satyavati,shantanu).
married(shantanu,ganga).
married(ambika,vichitravirya).
married(ambalika,vichitravirya).
married(gandhari ,dhritrashtra).
married(dushala,jayadratha).
married(draupadi,arjun).
married(draupadi,bheema).
married(draupadi,yudhisthir).
married(draupadi,nakul).
married(draupadi,sahdev).
married(subhadra,arjun).
married(rohini,vasudeva).
married(devaki,vasudeva).
married(uttara,abhimanyu).
married(kripi,dronacharya).
married(bhanumati,duryodhan).

marry(X,Y):-married(X,Y).
marry(X,Y):-married(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%% child relationships   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child(vidur,vyasa).
child(vidur,parishrami).
child(vichitravirya,shantanu).
child(vichitravirya,satyavati).

child(bheeshma,ganga).
child(bheeshma,shantanu).

child(shakuni,subala).
child(gandhari,subala).
child(shakuni,sudarma).
child(gandhari,sudarma).

child(duryodhan,gandhari).
child(duryodhan,dhritrashtra).
child(dushasn,gandhari).
child(dushasn,dhritrashtra).
child(dushala,gandhari).
child(dushala,dhritrashtra).
child(vikrana,gandhari).
child(vikrana,dhritrashtra).

child(draupadi,drupada).
child(draupadi,agni).
child(dhrishtadyumma,drupada).
child(dhrishtadyumma,agni).
child(shikandi,drupada).

child(ulook,shakuni).

child(yuyutsu,sukhada).
child(yuyutsu,dhritrashtra).

child(vyasa,satyavati).
child(vyasa,parashara).

child(dhritrashtra,ambika).
child(dhritrashtra,vyasa).

child(pandu,vyasa).
child(pandu,ambalika).

child(yudhisthir,pandu).
child(yudhisthir,kunti).

child(bheema,pandu).
child(bheema,kunti).

child(arjun,pandu).
child(arjun,kunti).

child(nakul,pandu).
child(nakul,madri).

child(sahdev,pandu).
child(sahdev,madri).

child(karana,suryadeva).
child(karana,kunti).

child(ghatotkatch,bheema).
child(ghatotkatch,hidimba).

child(abhimanyu,subhadra).
child(abhimanyu,arjun).

child(subhadra, vasudeva).
child(subhadra,rohini).

child(uttara,virata).

child(prikshit,uttara).
child(prikshit,abhimanyu).

child(aswathama,dronacharya).
child(aswathama,kripi).

child(kripi,sharadwan).
child(kripi,janpadi).
child(kripacharya,sharadwan).
child(kripacharya,janpadi).

child(lakshmana,duryodhan).
child(lakshmana,bhanumati).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

father(X,Y):-		male(X),child(Y,X).

mother(X,Y):-		female(X),child(Y,X).

truesibling(X,Y):-	mother(Z,X),						%%%%%%%%%%%%  Both parents common.
					mother(Z,Y),
					father(W,X),
					father(W,Y),
					(X \== Y).

halfsibling(X,Y):-	mother(Z,X),         				%%%%%%%%%%%%  Exactly one parent common.
					mother(Z,Y),
					father(W,X),
					%%%%%%not(father(W,Y)).
					father(W2,Y),
					( W \== W2).

halfsibling(X,Y):-	father(W,X),
					father(W,Y),
					mother(Z,X),
					%%%%%%%%%%%%%%%%not(mother(Z,Y)).
					mother(Z2,Y),
					( Z2 \== Z ).

sibling(X,Y):-	halfsibling(X,Y).						%%%%%%%%%%%%  Atleast one parent common(Half Sibling + True Sibling).
sibling(X,Y):-	truesibling(X,Y).

brother(X,Y):-	male(X),
				truesibling(X,Y).                       %%%%%%%%%%%% Male X having both parents common with Y.

sister(X,Y):-	female(X),								%%%%%%%%%%%% Female Y having both parents common with X.
				truesibling(X,Y).

step_father(X,Y):-	male(X),
					marry(X,Z),
					mother(Z,Y),
					child(Y,W),
					( X \== W ).

step_mother(X,Y):-	female(X),
					marry(X,Z),
					father(Z,Y),
					child(Y,W),
					( X \== W ).

half_brother(X,Y):-	male(X),							%%%%%%%%%%%%% Male Y having exactly one parent common with X.
					halfsibling(X,Y).

half_sister(X,Y):-	female(X),							%%%%%%%%%%%%% Female Y having exactly one parent common with X.
					halfsibling(X,Y).

uncleaunt(X,Y):-father(P,Y),
				sibling(P,X).
uncleaunt(X,Y):-mother(Z,Y),
				sibling(Z,X).
uncleaunt(X,Y):-father(Z,Y),
				sibling(Z,W),
				marry(W,X).
uncleaunt(X,Y):-mother(Z,Y),
				sibling(Z,W),
				marry(W,X).

uncle(X,Y):-	male(X),								%%%%%%%%%%%%%  includes 'chacha', 'mama' , 'fufa', 'mosa' etc 
				uncleaunt(X,Y).

aunt(X,Y):-		female(X),								%%%%%%%%%%%%%  includes 'chachi', 'mami' , 'buva', 'mosi' etc
				uncleaunt(X,Y).

sibling_in_law(X,Y):-marry(Y,Z),
					sibling(Z,X).
sibling_in_law(X,Y):-sibling(Y,Z),
					marry(Z,X).

sister_in_law(X,Y):-female(X),                          %%%%%%%%%%%%% The sister of one's spouse.
					sibling_in_law(X,Y).				%%%%%%%%%%%%% The wife of one's brother.

brother_in_law(X,Y):-male(X),							%%%%%%%%%%%%% The brother of one's spouse.
					sibling_in_law(X,Y).				%%%%%%%%%%%%% The husband of one's sister.

father_in_law(X,Y):-male(X),
					marry(Y,Z),child(Z,X).

ancester(X,Y):-		child(Y,X).
ancester(X,Y):-		child(Z,X),
					ancester(Z,Y).

descendant(X,Y):-	ancester(Y,X).

cousin(X,Y):-		child(X,P1),		                %%%%%%%%%%%%% a child of one's uncle or aunt.				
					child(Y,P2),
					sibling(P1,P2).


grandparent(X,Y):-	child(Z,X),
					child(Y,Z).

nthcousin(X,Y):-	cousin(X,Y).
nthcousin(X,Y):-	child(X,P1),			
					child(Y,P2),
					nthcousin(P1,P2).

niece(X,Y):-female(X),uncleaunt(Y,X).                   %%%%%%%% daughter of one's sibling.
nephew(X,Y):-male(X),uncleaunt(Y,X).					%%%%%%%% son of one's sibling.