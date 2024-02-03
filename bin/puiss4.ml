(*
Copyright 2024 DivDE divde@musicociel.fr

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Graphics ;;
open List ;;
open Array ;;
open Random ;;
open Sys ;;
let vect_length=length;;
let list_length=List.length;;
let do_list=List.iter;;
let make_vect=Array.make;;
let sizey=570;;



(* Fonctions de base ind�pendantes (qui devraient d�j� faire partie de CAML ! *)
let draw_rect x y l h =
	moveto x y ;
	lineto (x+l) y ;
	lineto (x+l) (y+h) ;
	lineto x (y+h) ;
	lineto x y ;;

let draw_poly v =
	moveto (fst v.(0)) (snd v.(0)) ;
	for x = 0 to vect_length(v) - 1 do (lineto (fst v.(x)) (snd v.(x))) done ;;

let ecrire_dans_cadre texte couleur x y larg haut =
	set_color white;
	fill_rect x y larg haut ;
	set_color couleur;
	moveto (x+(larg-fst(text_size texte))/2) (y+(haut-snd(text_size texte))/2);
	draw_string texte ;;

(*Syst�me de contr�les graphiques: syst�me r�utilisable dans d'autres programmes*)
let quitter=ref false ;;
let rien () = () ;; (*la fonction qui ne fait rien !*)
type ctrl={pos : (int*int) ; dim : (int*int) ; sourisdessus : unit->unit ; sourisplusdessus : unit->unit ; clicappui : unit->unit ; clicenvoi : unit->unit ; clicrelache : unit->unit} ;;
type clavier={lettre : char ; appel : unit->unit} ;;
let ctrlvide={pos = 0,0; dim = 0,0; sourisdessus=rien; sourisplusdessus=rien; clicappui=rien ; clicenvoi=rien ; clicrelache=rien};;
let sourissurctrl (souris:(int*int)) (ctrl:ctrl) = ( (fst(souris)>fst(ctrl.pos)) && (snd(souris)>snd(ctrl.pos)) && (fst(souris)<(fst(ctrl.pos)+fst(ctrl.dim))) && (snd(souris)<(snd(ctrl.pos)+snd(ctrl.dim)))) ;;
let attend listectrl listetouches =
	let eventr=ref (wait_next_event[Poll]) and eventa=ref (wait_next_event[Poll]) and ctrlprec=ref ctrlvide in
	quitter:=false ;
	while (not (!quitter))
	do
		eventa:=!eventr; eventr:=wait_next_event[Button_down; Button_up; Key_pressed; Mouse_motion];
		if (!eventr).keypressed then
			for i = 0 to (vect_length(listetouches)-1) do if (!eventr).key=listetouches.(i).lettre then listetouches.(i).appel() done ; (*(print_int (int_of_char !eventr.key)) ; print_newline() );*)
		if not (sourissurctrl ((!eventr).mouse_x,(!eventr).mouse_y) !ctrlprec) then (*si la souris a quitt� la zone*)
		begin
			let i = ref 0 in
			if (!eventa).button then (!ctrlprec).clicrelache() ; (*on avertit que la souris n'appuie plus sur le bouton*)
			(!ctrlprec).sourisplusdessus() ; (*on avertit que la souris n'est plus dessus*)
			while (!i<vect_length(listectrl) && (not (sourissurctrl ((!eventr).mouse_x,(!eventr).mouse_y) listectrl.(!i)))) (*on recherche une nouvelle zone*)
			do
				i:= !i+1;
			done ;
			if !i<vect_length(listectrl) then (*on en a trouv� une*)
			begin
				ctrlprec:=listectrl.(!i) ; (*elle remplace la pr�c�dente*)
				(!ctrlprec).sourisdessus() ; (*on avertit que la souris est dessus*)
				if (!eventr).button then (!ctrlprec).clicappui() ; (*on avertit le nouveau contr�le que la souris est appuy�e*)
			end
			else ctrlprec:=ctrlvide;
		end
		else (*la souris n'a pas quitt� la zone (m�me zone qu'avant) *)
		begin
			if (!eventa).button && not (!eventr).button then (*bouton rel�ch�*)
			begin
				(!ctrlprec).clicenvoi();
				(!ctrlprec).clicrelache()
			end ;
			if not (!eventa).button && (!eventr).button then (*bouton appuy�*)
				(!ctrlprec).clicappui();
		end ;
	done
	;;

(*Param�tres graphiques: r�glages possibles suivant la configuration*)
let larg=640 ;; (*largeur de la zone de travail*)
let haut=480 ;; (*hauteur de la zone de travail*)
let decy=50;; (*d�calage vertical depuis le haut pour que l'affichage soit optimal d�s le d�but*)

(*Param�tres graphiques calcul�s*)
let esp=larg*7/640 ;;
let cigx=10;; (*abscisse du point inf�rieur gauche de la zone de travail*)
let cigy=sizey-haut-decy ;; (*ordonn�e du point inf�rieur gauche de la zone de travail*)
(*Pour le plateau de jeu*)
let hcarac = 19 ;;
let largcases=larg*58/640 ;;
let largplateau=7*largcases+8*esp ;;
let hautplateau=6*largcases+7*esp ;;
let cigxcadreinfos=cigx+esp+largplateau+esp ;;
let cigycadreinfos=cigy+esp ;;
let largcadreinfos=larg-largplateau-(3*esp) ;;
let hautcadreinfos=haut-(2*esp) ;;
let jetonx=cigxcadreinfos+largcadreinfos/2 ;;
let jetony=cigycadreinfos+hautcadreinfos-2*hcarac-largcases/2-esp ;;

(*Dessine la zone de travail (ce qui n'est pas dans cette zone n'est pas utilis�*)
let dessinerzone () =
	set_color black ; draw_rect cigx cigy larg haut ; for i=1 to haut-1 do set_color (rgb 255 ((255*i)/haut) ((255*i)/haut)) ; moveto (cigx+1) (cigy+i) ; lineto (cigx+larg -1) (cigy+i) done ;;

let dessinercase couleur (x,y) =
	set_color (match(couleur) with | 0 -> white | 1 -> red | 2 -> yellow | _ -> assert false) ;
	fill_circle (cigx+esp+esp+(largcases+esp)*x+largcases/2) (cigy+esp+esp+(largcases+esp)*y+largcases/2) (largcases/2) ;
	set_color black ;
	draw_circle (cigx+esp+esp+(largcases+esp)*x+largcases/2) (cigy+esp+esp+(largcases+esp)*y+largcases/2) (largcases/2) ;;

let dessinejeton couleur =
	set_color (match(couleur) with | 0 -> white | 1 -> red | 2 -> yellow | _ -> assert false) ;
	fill_circle jetonx jetony (largcases/2) ;
	set_color black ;
	draw_circle jetonx jetony (largcases/2) ;;

let domain = "musicociel.fr";;
let at = "@";;
let dessinercadreinfos () =
	ecrire_dans_cadre "Puissance 4" red (cigxcadreinfos+1) (cigycadreinfos+hautcadreinfos-2*hcarac) (largcadreinfos-1) (2*hcarac) ;
        ecrire_dans_cadre ("divde"^at^domain) red (cigxcadreinfos+1) (cigycadreinfos) (largcadreinfos-1) hcarac ;
	ecrire_dans_cadre "Programm� par" red (cigxcadreinfos+1) (cigycadreinfos+hcarac) (largcadreinfos-1) (hcarac) ;
	set_color blue ; draw_rect cigxcadreinfos cigycadreinfos largcadreinfos hautcadreinfos ;
	dessinejeton 2
	;;

let affichercommentaire texte = ecrire_dans_cadre texte blue cigx (cigy-esp-hcarac) larg hcarac ;;

let clignotercases couleur listecases =
	for i = 0 to 12 do do_list (dessinercase (couleur * ((i+1) mod 2))) listecases (*; sound (([|440 ; 660 ; 880; 660|]).(i mod 4)) 100 ;*) done;;


(*Les proc�dures de test s'il y a puissance 4*)
let jeuvide ()= make_matrix 7 6 0;;
let verifpuiss4vert jeu x y =
	let c = ref 0 and ny = ref y and jetons = ref [] in
	while !ny>=0 && jeu.(x).(!ny) = jeu.(x).(y) do jetons:= (x,!ny):: (!jetons) ; c:= !c+1 ; ny:= !ny-1 done;
	if !c>=4 then !jetons else [];;
let verifpuiss4horiz jeu x y =
	let c = ref 0 and nx = ref x and jetons = ref [] in
	(* on compte d'abord � gauche*)
	while !nx>=0 && jeu.(!nx).(y) = jeu.(x).(y) do jetons:= (!nx,y):: (!jetons) ; c:= !c+1 ; nx:= !nx-1 done;
	(* puis � droite*)
	nx:= x+1 ;
	while !nx<=6 && jeu.(!nx).(y) = jeu.(x).(y) do jetons:= (!nx,y):: (!jetons) ; c:= !c+1 ; nx:= !nx+1 done;
	if !c>=4 then !jetons else [];;
let verifpuiss4diagHBGD jeu x y =
	let c = ref 0 and nx = ref x and ny = ref y and jetons = ref [] in
	(* on compte d'abord � gauche*)
	while !nx>=0 && !ny<=5 && jeu.(!nx).(!ny) = jeu.(x).(y) do jetons:= (!nx,!ny):: (!jetons) ; c:= !c+1 ; nx:= !nx-1; ny:= !ny+1 done;
	(* puis � droite*)
	nx:= x+1 ;
	ny:= y-1 ;
	while !nx<=6 && !ny>=0 && jeu.(!nx).(!ny) = jeu.(x).(y) do jetons:= (!nx,!ny):: (!jetons) ; c:= !c+1 ; nx:= !nx+1; ny:= !ny-1 done;
	if !c>=4 then !jetons else [];;
let verifpuiss4diagBHGD jeu x y =
	let c = ref 0 and nx = ref x and ny = ref y and jetons = ref [] in
	(* on compte d'abord � gauche*)
	while !nx>=0 && !ny>=0 && jeu.(!nx).(!ny) = jeu.(x).(y) do jetons:= (!nx,!ny):: (!jetons) ; c:= !c+1 ; nx:= !nx-1; ny:= !ny-1 done;
	(* puis � droite*)
	nx:= x+1 ;
	ny:= y+1 ;
	while !nx<=6 && !ny<=5 && jeu.(!nx).(!ny) = jeu.(x).(y) do jetons:= (!nx,!ny):: (!jetons) ; c:= !c+1 ; nx:= !nx+1; ny:= !ny+1 done;
	if !c>=4 then !jetons else [];;
let verifpuiss4 jeu x y = (((verifpuiss4horiz jeu x y) @ (verifpuiss4vert jeu x y))
	@ ((verifpuiss4diagHBGD jeu x y) @ (verifpuiss4diagBHGD jeu x y)) );;
(*let testpuiss4 jeu x y = (list_length (verifpuiss4 jeu x y))<> 0 ;;*)
let verifcasesvides jeu =
	let jetons = ref [] and n=ref 0 in
		while (!n<=6 && jeu.(!n).(5)<>0) do jetons:=(!n,5):: (!jetons) ; n:= !n+1 done ;
		if !n>6 then !jetons else [] ;;

let copiejeu jeu =
	let nouvjeu = jeuvide () in
	for x = 0 to 6 do for y = 0 to 5 do nouvjeu.(x).(y)<-jeu.(x).(y) done done; nouvjeu ;;

type evalcoup = {colonne : int ; (*num�ro de la colonne o� on joue*)
                  joueur : int ; (*indique si le coup est jou� par les jaunes (2) ou les rouges (1) *)
            couppossible : bool ; (*indique si le coup est possible (false si colonne pleine) *)
                profeval : int ; (*indique la profondeur d'�valuation effectu�e (nombre de coups jusqu'� la fin si finpartie=true) *)
               finpartie : bool (*true si l'�valuation m�ne � une fin de partie, le signe de notefinale est positif si joueur gagne et n�gatif si perd, notefinale=0 si partie nulle*) ;
              notefinale : int ;
            noteposition : int (*prend la somme des notes des positions jusqu'� profeval*)
} ;;


let gagne a = (a.finpartie && (a.notefinale > 0));;
let perd a = (a.finpartie && (a.notefinale < 0));;
(*comparaison de deux coups: se place selon le pt de vue de a.joueur suppos� �gal � b.joueur*)
let eststrictementmeilleurque a b =
	if not a.couppossible then false (*un coup impossible ne peut pas �tre strictement meilleur qu'un coup quelconque*)
	else if not b.couppossible then true (*si b impossible et a possible alors le coup possible est mieux*)
	else begin
		if (gagne a) && (gagne b) then (*les deux coups remportent la victoire: on prend le plus rapide ou le plus joli*)
			(a.profeval < b.profeval) || ((a.profeval = b.profeval) && (a.notefinale > b.notefinale))
		else if (gagne a) && not (gagne b) then true
		else if (gagne b) && not (gagne a) then false
		else if (perd a) && (perd b) then (*les deux coups donnent la d�faite: on prend la plus lente et la moins jolie*)
			(a.profeval > b.profeval) || ((a.profeval = b.profeval) && (a.notefinale > b.notefinale))
		else if (perd a) && not (perd b) then false
		else if (perd b) && not (perd a) then true
		else if a.finpartie && b.finpartie then (*les deux coups donnent le nul: on prend le plus lent*)
			(a.profeval > b.profeval)
		else if (a.finpartie) && (not b.finpartie) then false
		else if (b.finpartie) && (not a.finpartie) then true
		else a.noteposition > b.noteposition
	end;;

let remonteeval ncol eval valpos =
	    {colonne = ncol ;
              joueur = 1 + (eval.joueur mod 2) ; (*le joueur change � chaque fois*)
        couppossible = eval.couppossible ;
            profeval = eval.profeval + 1 ; (*la profondeur est augment�e de 1*)
           finpartie = eval.finpartie;
          notefinale = -eval.notefinale ; (*on prend l'oppos� des �valuations pour changer de point de vue*)
        noteposition = valpos} ;;

(*Les proc�dures d'�valuation d'une position*)
let evalposvert jeu x y =
	let c = ref 0 and d = ref 0 and ny = ref y in
	while !ny>=0 && (jeu.(x).(!ny) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1 ; if (jeu.(x).(!ny)=jeu.(x).(y)) then d:= !d+1 ; ny:= !ny-1 done;
	if !c>=4 then !c + !d*3 else 0 ;;
let evalposhoriz jeu x y =
	let c = ref 0 and d = ref 0 and nx = ref x in
	(* on compte d'abord � gauche*)
	while !nx>=0 && (jeu.(!nx).(y) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1; if (jeu.(!nx).(y)=jeu.(x).(y)) then d:= !d+1 ; nx:= !nx-1 done;
	(* puis � droite*)
	nx:= x+1 ;
	while !nx<=6 && (jeu.(!nx).(y) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1; if (jeu.(!nx).(y)=jeu.(x).(y)) then d:= !d+1 ; nx:= !nx+1 done;
	if !c>=4 then !c + !d*3 else 0 ;;
let evalposdiagHBGD jeu x y =
	let c = ref 0 and d = ref 0 and nx = ref x and ny = ref y in
	(* on compte d'abord � gauche*)
	while !nx>=0 && !ny<=5 && (jeu.(!nx).(!ny) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1 ; if (jeu.(!nx).(!ny)=jeu.(x).(y)) then d:= !d+1 ; nx:= !nx-1; ny:= !ny+1 done;
	(* puis � droite*)
	nx:= x+1 ;
	ny:= y-1 ;
	while !nx<=6 && !ny>=0 && (jeu.(!nx).(!ny) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1 ; if (jeu.(!nx).(!ny)=jeu.(x).(y)) then d:= !d+1 ; nx:= !nx+1; ny:= !ny-1 done;
	if !c>=4 then !c + !d*3 else 0 ;;
let evalposdiagBHGD jeu x y =
	let c = ref 0 and d = ref 0 and nx = ref x and ny = ref y in
	(* on compte d'abord � gauche*)
	while !nx>=0 && !ny>=0 && (jeu.(!nx).(!ny) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1 ; if (jeu.(!nx).(!ny)=jeu.(x).(y)) then d:= !d+1 ; nx:= !nx-1; ny:= !ny-1 done;
	(* puis � droite*)
	nx:= x+1 ;
	ny:= y+1 ;
	while !nx<=6 && !ny<=5 && (jeu.(!nx).(!ny) <> (1 + (jeu.(x).(y) mod 2))) do c:= !c+1 ; if (jeu.(!nx).(!ny)=jeu.(x).(y)) then d:= !d+1 ; nx:= !nx+1; ny:= !ny+1 done;
	if !c>=4 then !c + !d*3 else 0 ;;

let evalueposition jeu n y =
	let njeu = copiejeu jeu and res= ref 0 in
	(*on fait ici plusieurs v�rifications: le coup effectu� permet-il � l'autre d'emp�cher un puissance 4 (mauvais) ?
	le coup effectu� permet-il de lier efficacement un certain nombre de jeton ?*)
	if y <5 then (*si notre pion n'est pas tout en haut*)
	begin
		njeu.(n).(y+1)<-njeu.(n).(y); (*on place le pion au-dessus de l� o� on l'a mis*)
		njeu.(n).(y)<-0; (* on enl�ve le pion l� o� on l'a mis (pour ne pas comptabiliser un puiss 4 vertical) *)
		let l = list_length(verifpuiss4 njeu n (y+1)) in
		if (l <> 0) then
			res := -l * 5; (*on est en train d'offrir � l'autre la possibilit� d'enlever notre puissance 4*)
	end ;
	if !res = 0 then
	res := (evalposvert jeu n y) + (evalposhoriz jeu n y) + (evalposdiagHBGD jeu n y) + (evalposdiagBHGD jeu n y);
	!res
	;;

let obtientordre () =
	let alea1 = ref (int 7) and alea2 = ref (int 7) and compte=ref (int 7) and tmp=ref 0 and ordre=[|3;4;2;5;1;6;0|] in
	for _ = 0 to !compte do tmp:=ordre.(!alea1) ; ordre.(!alea1)<-ordre.(!alea2); ordre.(!alea2)<- !tmp; alea1:= int 7; alea2:=int 7 done ; ordre;;

let rec evaluecoup jeu meilleurcoupactuel n profondeur =
	let couleuradverse=(1+(meilleurcoupactuel.joueur mod 2)) and njeu=(copiejeu jeu) in
		if njeu.(n).(5)<>0 then (*le coup n'est pas possible*)
			({colonne = n ;
	                   joueur = meilleurcoupactuel.joueur ;
                     couppossible = false ;
                         profeval = 0 ;
                        finpartie = false;
                       notefinale = 0 ;
                     noteposition = 0 })
		else (*le coup est possible*)
		begin
		let y=ref 0 and i = ref 0 in
			while njeu.(n).(!y)<>0 do y:= !y+1 done;
			njeu.(n).(!y)<-meilleurcoupactuel.joueur ; (*on effectue le coup*)
			i:=list_length(verifpuiss4 njeu n !y);
			let valpos = (evalueposition njeu n !y) in
			if !i <> 0 then {colonne = n ; joueur = meilleurcoupactuel.joueur ; couppossible = true ; profeval=1 ; finpartie = true ; notefinale = !i; noteposition=valpos}
			else if profondeur=0 then {colonne = n ; joueur = meilleurcoupactuel.joueur ; couppossible = true ; profeval=1 ; finpartie=false; notefinale = 0; noteposition = valpos}
			else
			begin

	let meilleurcoupadverse =  ref {colonne = -1 ;
					 joueur = couleuradverse ;
				   couppossible = false ;
				       profeval = 0 ;
				      finpartie = false ;
				     notefinale = 0 ;
				   noteposition = 0} in (*on cr�e un coup compl�tement nul qui va servir de base pour avoir mieux*)
		let meilleurcoupremonte = ref (remonteeval (-1) !meilleurcoupadverse valpos) and coup = ref !meilleurcoupadverse
		and ordre = obtientordre() and a = ref 0 in
			while (!a < 7)
			do
				coup:= (evaluecoup njeu !meilleurcoupadverse ordre.(!a) (profondeur-1)) ;
				(*on vient d'�valuer le coup num�ro a*)
				(*si ce coup est meilleur (pour couleuradverse) que le pr�c�dent, alors l'adversaire le pr�f�rera*)
				if (eststrictementmeilleurque (!coup) (!meilleurcoupadverse)) then
				begin
					meilleurcoupadverse:= !coup ;
					meilleurcoupremonte:= (remonteeval n (!coup) valpos);
					if (not (eststrictementmeilleurque (!meilleurcoupremonte) meilleurcoupactuel)) then
						a:= 7  (*on quitte la boucle: cela ne sert � rien (on ne peut pas faire un meilleur coup qu'avant) *)
				end ;
				a:= !a +1
			done;
			if not (!meilleurcoupremonte.couppossible) then
				(*aucun des sous-coups n'est possible: il s'agit donc du dernier coup � jouer: on le joue !*)
				meilleurcoupremonte:= {colonne = n; joueur=meilleurcoupactuel.joueur; couppossible=true; profeval=1; finpartie=true;notefinale=0;noteposition=valpos};
			(!meilleurcoupremonte)
			end
		end ;;

let determineprofondeur jeu =
	let c = ref 0 in
	for a = 0 to 6 do if jeu.(a).(5)<>0 then c:= !c+1 done ; (*compte le nombre de colonnes finies*)
	!c + 8 ;;

let ordijoue commenter jeu couleur =
	let lemeilleurcoup = ref ({colonne = 0;
			joueur = couleur ;
			couppossible = false ;
			profeval = 0 ;
			finpartie = false ;
			notefinale = 0 ;
			noteposition = 0}) in
		let coup = ref (!lemeilleurcoup) and ordre=obtientordre () and prof = (determineprofondeur jeu) in
		for a = 0 to 6 do
			coup:= (evaluecoup jeu !lemeilleurcoup ordre.(a) prof);
			if eststrictementmeilleurque !coup !lemeilleurcoup then lemeilleurcoup:= !coup
			done ;
		commenter !lemeilleurcoup ;
	 (!lemeilleurcoup).colonne ;;

(*Le jeu lui-m�me*)
let jeu=jeuvide ();;
let couleuractuelle=ref 2 ;;
let fini=ref false;;
let joueurjaune=ref 0 ;; (*0=utilisateur ; 1=ordinateur*)
let joueurrouge=ref 1 ;; (*0=utilisateur ; 1=ordinateur*)
let historique=ref [];;

let annulercoup () =
	if !historique = [] then ()
	else
	begin
		let a = (hd !historique) and b = ref 5 in
			while !b >= 0 do
				if jeu.(a).(!b)<>0 then
					begin
						jeu.(a).(!b)<-0 ;
						dessinercase 0 (a,!b) ;
						b:=-1 ;
					end ;
				b:= !b-1
			done;
			couleuractuelle:= 1 + (!couleuractuelle mod 2);
			dessinejeton !couleuractuelle;
			fini:=false;
			affichercommentaire "";
			historique:= tl !historique
	end ;;

let copierjeu () =
	let hist = ref !historique and texte = ref "" in
	while (!hist<>[]) do texte:= string_of_int(1 + (hd !hist)) ^ !texte ; hist:=tl !hist done;
	print_string "Liste des coups:"; print_newline() ; print_string !texte ; print_newline() ;;

let nomcouleur couleur = match couleur with |1 -> "les rouges" |2->"les jaunes" | _ -> assert false ;;

let commenter coup =
	let texte = (ref ("Jou� colonne " ^ string_of_int(1+coup.colonne))) in
	if coup.finpartie then
	begin
		texte:= !texte ^ " - Fin de la partie dans " ^ string_of_int(coup.profeval-1) ^ " coup(s): " ;
		if coup.notefinale = 0 then
			texte:= !texte ^ "partie nulle !"
		else
			texte:= !texte ^ (nomcouleur !couleuractuelle) ^ (if coup.notefinale > 0 then " gagnent !" else " perdent !");
	end ;
	affichercommentaire !texte ;;

(*Modification du jeu*)
let nouveaujeu () =
	for x = 0 to 6 do for y=0 to 5 do (jeu.(x)).(y)<-0 done done ; couleuractuelle:=2 ; fini:=false ; historique:=[];;
let coljouable x = (jeu.(x).(5)=0 && not !fini) ;;

let jeuordi () = (not !fini) && ((!couleuractuelle=2 && !joueurjaune=1) || (!couleuractuelle=1 && !joueurrouge=1)) ;;

let rec clicfleche n () =
	if coljouable n then
	begin
		let i = ref 0 and l = ref [] in
			historique:= n :: !historique ;
			while jeu.(n).(!i)<>0 do i:= !i+1 done ;
			(jeu.(n)).(!i)<- !couleuractuelle;
			dessinercase !couleuractuelle(n,!i) ;
			l:=verifpuiss4 jeu n !i;
			if list_length(!l) <>0 then
			begin
				clignotercases !couleuractuelle !l ;
				affichercommentaire ("Fin de la partie: " ^ (nomcouleur !couleuractuelle)^" ont gagn� !") ;
				fini:=true
			end
			else if !i = 5 then (*on v�rifie qu'il reste des cases vides*)
			begin
				l:=verifcasesvides jeu;
				if list_length(!l) <> 0 then
				begin
					affichercommentaire ("Fin de la partie: partie nulle !") ;
					fini:=true
				end
			end ;
		couleuractuelle:= 1+ ((!couleuractuelle) mod 2) ;
		dessinejeton !couleuractuelle ;
		if jeuordi () then clicfleche (ordijoue commenter jeu !couleuractuelle) ()
	end ;;

let jouer = (function ()->(if not !fini then begin affichercommentaire "" ; clicfleche (ordijoue commenter jeu !couleuractuelle) () end));;

(*Liste des contr�les pour ce programme*)
let nctrl=ref 0;;
let listectrl=make_vect 14 ctrlvide ;; (*la liste des contr�les*)
let listetouches=make_vect 13 {lettre = char_of_int(27); appel=(function ()->(quitter:=true))} ;; (*la liste des touches*)
listetouches.(1)<-{lettre=char_of_int(32) ; appel=(jouer)};;
listetouches.(2)<-{lettre='p' ; appel=annulercoup};;
listetouches.(3)<-{lettre='1' ; appel=(clicfleche 0)};;
listetouches.(4)<-{lettre='2' ; appel=(clicfleche 1)};;
listetouches.(5)<-{lettre='3' ; appel=(clicfleche 2)};;
listetouches.(6)<-{lettre='4' ; appel=(clicfleche 3)};;
listetouches.(7)<-{lettre='5' ; appel=(clicfleche 4)};;
listetouches.(8)<-{lettre='6' ; appel=(clicfleche 5)};;
listetouches.(9)<-{lettre='7' ; appel=(clicfleche 6)};;
(*la d�claration 10 est plus bas*)
listetouches.(11)<-{lettre='c' ; appel=(copierjeu)};;
(*la d�claration 12 est plus bas*)

let ajoutectrl (ctrl:ctrl) = listectrl.(!nctrl)<-ctrl ; nctrl:= !nctrl+1 ;;

let dessinerplateau () =
	set_color blue ; fill_rect (cigx+esp) (cigy+esp) largplateau hautplateau ;
	set_color black ; draw_rect (cigx+esp) (cigy+esp) largplateau hautplateau ;
	for x = 0 to 6 do (for y=0 to 5 do dessinercase jeu.(x).(y) (x,y) done) done ;;

let dessinerfleche n etat () =
	let x=(cigx+esp+esp+(largcases+esp)*n) and y=cigy+esp+hautplateau+esp in
	if coljouable n then
	begin
		set_color (match etat with | 0-> white | 1-> cyan | 2-> blue | _ -> assert false);
		fill_poly [|x+largcases/4,y+largcases;x+3*largcases/4,y+largcases;x+3*largcases/4,y+largcases/2;x+largcases,y+largcases/2;x+largcases/2,y;x,y+largcases/2;x+largcases/4,y+largcases/2; |] ;
		set_color (match etat with | 0-> rgb 100 100 100 |1 -> blue | 2-> cyan | _ -> assert false );
		draw_poly [|x+largcases/4,y+largcases;x+3*largcases/4,y+largcases;x+3*largcases/4,y+largcases/2;x+largcases,y+largcases/2;x+largcases/2,y;x,y+largcases/2;x+largcases/4,y+largcases/2; x+largcases/4,y+largcases; |]
	end
	else
	begin
		set_color (rgb 250 250 250);
		fill_poly [|x+largcases/4,y+largcases;x+3*largcases/4,y+largcases;x+3*largcases/4,y+largcases/2;x+largcases,y+largcases/2;x+largcases/2,y;x,y+largcases/2;x+largcases/4,y+largcases/2; |] ;
	end ;;

let ajouterctrlfleche n =
	let x=(cigx+esp+esp+(largcases+esp)*n) and y=cigy+esp+hautplateau+esp in
		ajoutectrl {pos = x,y; dim = largcases,largcases; sourisdessus=(dessinerfleche n 1); sourisplusdessus=(dessinerfleche n 0); clicappui=(dessinerfleche n 2) ; clicenvoi=(clicfleche n) ; clicrelache=(dessinerfleche n 1)};
		dessinerfleche n 0 () ;;

let dessinerjetonjoueur n couleur =
	let xx=(cigxcadreinfos+esp+hcarac*3/4) and yy=(cigycadreinfos+hautcadreinfos-2*hcarac-esp-largcases-(n+1)*(hcarac*3/2+esp)+hcarac*3/4) in
	set_color (match(couleur) with | 0 -> white | 1 -> red | 2 -> yellow | _ -> assert false) ;
	fill_circle xx yy (hcarac*3/4) ;
	set_color black ;
	draw_circle xx yy (hcarac*3/4) ;;

let dessinerjetonjoueurs () =
	dessinerjetonjoueur 2 (if !joueurjaune = 0 then 2 else 0);
	dessinerjetonjoueur 3 (if !joueurjaune = 1 then 2 else 0);
	dessinerjetonjoueur 5 (if !joueurrouge = 0 then 1 else 0);
	dessinerjetonjoueur 6 (if !joueurrouge = 1 then 1 else 0) ;;

let dessinerbouton texte x n etat () =
	let xx=(cigxcadreinfos+esp+x) and yy=(cigycadreinfos+hautcadreinfos-2*hcarac-esp-largcases-(n+1)*(hcarac*3/2+esp)) in
		let l=cigx+larg-esp-esp-xx and h=hcarac*3/2 in
			set_color white;
			fill_rect xx yy l h;
			ecrire_dans_cadre texte (match etat with |0-> rgb 100 100 100 |1->blue |2->red |3->black | _ -> assert false) (xx+esp/2+(if etat=2 then 1 else 0)) (yy+esp/2-(if etat=2 then 1 else 0)) (l-esp) (h-esp);
			set_color (match etat with | 2 -> rgb 0 0 100 |1 -> blue | _ -> white ) ;
			draw_rect (xx+1) (yy+1) (l-2) (h-2);
			set_color (match etat with | 0-> rgb 100 100 100 | 2 ->blue |1 -> rgb 0 0 100 |3-> white | _ -> assert false ) ;
			draw_rect xx yy l h;;

let ajouterctrlbouton texte x n clic =
	let xx=(cigxcadreinfos+esp+x) and yy=(cigycadreinfos+hautcadreinfos-2*hcarac-esp-largcases-(n+1)*(hcarac*3/2+esp)) in
		let l=cigx+larg-esp-esp-xx and h=hcarac*3/2 in
			ajoutectrl {pos = xx,yy; dim = l,h; sourisdessus=(dessinerbouton texte x n 1); sourisplusdessus=(dessinerbouton texte x n 0); clicappui=(dessinerbouton texte x n 2) ; clicenvoi=clic ; clicrelache=(dessinerbouton texte x n 1)};
			dessinerbouton texte x n 0 ();;

let rec dessin () = (
	affichercommentaire "";
	dessinerzone();
	dessinercadreinfos();
	dessinerplateau();
	dessinerctrl();
	dessinejeton !couleuractuelle)
and dessinerctrl () =
	nctrl:=0;
	for i = 0 to 6 do ajouterctrlfleche i done ;
	ajouterctrlbouton "Nouveau jeu" 0 0 (function ()->(nouveaujeu();dessin())) ;
	dessinerbouton "Joueur jaune:" 0 1 3 () ;
	ajouterctrlbouton "Utilisateur" (hcarac*3/2+esp) 2 (function ()->(joueurjaune:=0; dessinerjetonjoueurs ())) ;
	ajouterctrlbouton "Ordinateur" (hcarac*3/2+esp) 3 (function ()->(joueurjaune:=1; dessinerjetonjoueurs ())) ;
	dessinerbouton "Joueur rouge:" 0 4 3 () ;
	ajouterctrlbouton "Utilisateur" (hcarac*3/2+esp) 5 (function ()->(joueurrouge:=0; dessinerjetonjoueurs ())) ;
	ajouterctrlbouton "Ordinateur" (hcarac*3/2+esp) 6 (function ()->(joueurrouge:=1; dessinerjetonjoueurs ())) ;
	ajouterctrlbouton "Jouer" 0 7 jouer ;
	ajouterctrlbouton "Quitter" 0 8 (function ()->(quitter:=true)) ;
	dessinerjetonjoueurs () ;;

listetouches.(10)<-{lettre='n' ; appel=(function ()->(nouveaujeu();dessin()))};;
listetouches.(12)<-{lettre='d' ; appel=(dessin)};;

let puiss4 () =
open_graph "" ;
resize_window (larg+50) (haut+70+20) ;
nouveaujeu() ;
dessin() ;
init (int_of_float (time())); (*initialisation de l'al�atoire*)
affichercommentaire "Derni�re mise � jour: 3 f�vrier 2024, code source: https://github.com/davdiv/puiss4";
attend listectrl listetouches ;;
puiss4 (); exit 0;;
