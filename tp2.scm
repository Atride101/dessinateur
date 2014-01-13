;; Fichier : "tp2.scm"

;;-----------------------------------------------------------------------------

;;cree une ligne entre deux vecteurs. le segmens cree est un ensemble de petits segments
(define ligne
  (lambda (depart arrivee)
    (lambda (x)
      (if (procedure? arrivee)
	  (if (null? (arrivee 0))
	      depart
	      (if (list? (arrivee 0))
		  (if (list? (segm-depart (arrivee 0)))
		      (append ((ligne depart (segm-depart (segm-depart (arrivee 0)))) x)
			      (arrivee 0))
		      (append ((ligne depart (segm-depart (arrivee 0))) x)
			      (arrivee 0)))
		  (let ((ligne-x (- (vect-x depart) (vect-x (arrivee 0))))
			(ligne-y (- (vect-y depart) (vect-y (arrivee 0)))))
		    (if (< (sqrt (+ (* ligne-x ligne-x) (* ligne-y ligne-y))) (/ 1 10))
			(list (segm depart (arrivee 0)))
			(let ((milieu-x (/ (+ (vect-x depart) (vect-x (arrivee 0))) 2))
			      (milieu-y (/ (+ (vect-y depart) (vect-y (arrivee 0))) 2)))
			  (let ((milieu (vect milieu-x milieu-y)))
			    (append ((ligne depart milieu) x)
				    ((ligne milieu (arrivee 0)) x))))))))
	  (if (null? arrivee)
	      depart
	      (if (list? arrivee)
		  (if (list? (segm-depart arrivee))
		      (append ((ligne depart (segm-depart (segm-depart arrivee))) x)
			      arrivee)
		      (append ((ligne depart (segm-depart arrivee)) x)
			      arrivee))
		  (let ((ligne-x (- (vect-x depart) (vect-x arrivee)))
			(ligne-y (- (vect-y depart) (vect-y arrivee))))
		    (if (< (sqrt (+ (* ligne-x ligne-x) (* ligne-y ligne-y))) (/ 1 10))
			(list (segm depart arrivee))
			(let ((milieu-x (/ (+ (vect-x depart) (vect-x arrivee)) 2))
			      (milieu-y (/ (+ (vect-y depart) (vect-y arrivee)) 2)))
			  (let ((milieu (vect milieu-x milieu-y)))
			    (append ((ligne depart milieu) x)
				    ((ligne milieu arrivee) x))))))))))))

;;cree une liste de segments a partir d'une liste de vecteurs
(define parcours->dessinateur
  (lambda (parcours)
	
	;;si la liste est nulle, retourner le  dessinateur vide
    (if (null? parcours)
	(ligne (vect 0 0) (vect 0 0))
	(foldr ligne '() parcours))))

(define ell
  (parcours->dessinateur
   (list (vect -1/2 1) (vect -1/2 -1) (vect 1/2 -1))))

(define vide (parcours->dessinateur '()))

(define triangle
  (parcours->dessinateur
   (list (vect -1 -1) (vect 0 1) (vect 1 -1) (vect -1 -1))))

;;vecteurs necessaires pour dessiner les chiffres
(define zero
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 0)))

(define un
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 1)))

(define deux
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 2)))

(define trois
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 3)))

(define quatre
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 4)))

(define cinq
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 5)))

(define six
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 6)))

(define sept
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 7)))

(define huit
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 8)))

(define neuf
  (parcours->dessinateur (vector-ref parcours-pour-chiffres 9)))

;;tourne le dessin dans le sens horaire
(define rotation 
  (lambda (theta dessinateur)
    (lambda (x)
      (map
       (lambda (i)
		;;converti le theta (degree) en radians pour les calculs
		(let ((theta2 (/ (* theta (* 2 3.1416)) 360)))
	 (segm
	  (vect	(+ (* (vect-x (segm-depart i))(cos theta2)) (* (vect-y (segm-depart i))(sin theta2)))
		(- (* (vect-y (segm-depart i))(cos theta2)) (* (vect-x (segm-depart i))(sin theta2))))
	  (vect	(+ (* (vect-x (segm-arrivee i))(cos theta2)) (* (vect-y (segm-arrivee i))(sin theta2)))
		(- (* (vect-y (segm-arrivee i))(cos theta2)) (* (vect-x (segm-arrivee i))(sin theta2)))))))
       (dessinateur 0)))))

;;deplace le dessin
;;il suffit de changer les vecteurs de tous les segments
(define translation 
  (lambda (deplacement-x deplacement-y dessinateur)
    (lambda (x)
      (map
       (lambda (i)
	 (segm
	  (vect	(+ (vect-x (segm-depart i)) deplacement-x)
		(+ (vect-y (segm-depart i)) deplacement-y))
	  (vect	(+ (vect-x (segm-arrivee i)) deplacement-x)
		(+ (vect-y (segm-arrivee i)) deplacement-y))))
       (dessinateur 0)))))

;;change la taille du dessin
;;il suffit de changer les vecteurs de tous les segments
(define reduction
  (lambda (facteur-x facteur-y dessinateur)
    (lambda (x)
      (map
       (lambda (i)
	 (segm
	  (vect	(* (vect-x (segm-depart i)) facteur-x)
		(* (vect-y (segm-depart i)) facteur-y))
	  (vect	(* (vect-x (segm-arrivee i)) facteur-x)
		(* (vect-y (segm-arrivee i)) facteur-y))))
       (dessinateur 0))))) 

;;donne un effet de loupe au dessin
(define loupe
  (lambda (facteur dessinateur)
	(lambda (x)
	  (map
	    (lambda (i)
		;;la valeur de 'm' varie selon le vecteur. m-depart est le 'm' pour le vecteur-depart du segment et m-arrivee pour le 'm' du vecteur arrivee
		;;nous appliquons simplement la formule
		  (let ((m-depart (/ (+ 1 facteur)
							 (+ 1 (* facteur (+ (* (vect-x (segm-depart i)) (vect-x (segm-depart i)))(* (vect-y (segm-depart i)) (vect-y (segm-depart i))))))))
				(m-arrivee (/ (+ 1 facteur)
							  (+ 1 (* facteur (+ (* (vect-x (segm-arrivee i)) (vect-x (segm-arrivee i)))(* (vect-y (segm-arrivee i)) (vect-y (segm-arrivee i)))))))))
				;;avec les m calcules, il suffit de modifie les vecteurs des segments
				(segm
					(vect (* (vect-x (segm-depart i)) m-depart)
						  (* (vect-y (segm-depart i)) m-depart))
					(vect (* (vect-x (segm-arrivee i)) m-arrivee)
						  (* (vect-y (segm-arrivee i)) m-arrivee)))))
		(dessinateur 0)))))

;;met deux dessins ensemble
(define superposition
  (lambda (dessinateur1 dessinateur2)
    (lambda (x)
      (append (dessinateur1 0) (dessinateur2 0)))))

;;met un dessin sur l'autre
(define pile
  (lambda (prop dessinateur1 dessinateur2)
    (lambda (x)
		;;mofidifie la taille des dessins pour respecte la proportion
		;;et deplacer le 2em dessin sur le haut du premier
      ((superposition (translation 0 (- prop 1) (reduction 1 prop dessinateur1))
		      (translation 0 prop (reduction 1 (- 1 prop) dessinateur2))) 0))))

;;met deux dessins cote-a-cote en respectant la proportion donnee
(define cote-a-cote
  (lambda (prop dessinateur1 dessinateur2)
    (lambda (x)
		;;mofidifie la taille des dessins pour respecte la proportion
		;;et deplacer le 2em dessin a droite du premier
      ((superposition (translation (- prop 1) 0 (reduction prop 1 dessinateur1))
		      (translation prop 0 (reduction (- 1 prop) 1 dessinateur2))) 0))))

;;dessine un entier donne en entree
(define entier->dessinateur
  (lambda (n)
    (lambda (x)
		;;si le nombre est un chiffre, dessiner avec les fonctions pour dessiner un chiffre
      (if (< n 10)
	  ((parcours->dessinateur (vector-ref parcours-pour-chiffres n)) 0)
	  ;;sinon, calculer l'entier le plus a droite du nombre
	  (let ((entier (modulo n 10))
		;;garder ce qui n'est pas l'entier
		(reste (/ (- n (modulo n 10)) 10)))
	    ((cote-a-cote (- 1 (/ 1 (ceiling (/ (log n) (log 10)))))
			;;dessiner le reste a gauche
			  (entier->dessinateur reste)
			  ;;dessiner l'entier a droite
			  (parcours->dessinateur (vector-ref parcours-pour-chiffres entier))) 0))))))

;;dessine un arbre a partie d'une liste
(define arbre->dessinateur
  (lambda (n)
    (lambda (x)
	  ;;cette fonction calcule la profondeur d'une liste (ie le nombre de listes imbriquees)
      (define depth 
	(lambda (y)
	  (if (or (null? y)
		  (and (not (list? y))
		       (not (pair? y))))
	      0
	      (max (+ 1 (depth (cdr y)))))))
	   ;;si la liste a une profondeur de 1
      (if (eq? 1 (depth n)) 
	  ;;on imprime seulement un arbre de hauteur 1
	  ((superposition (ligne (vect -1/2 -1) (vect 0 1)) 
			  (ligne (vect 0 1) (vect 1/2 -1))) 0)
	  ;;si la liste a une profondeur plus grande que 1
	  (if (and (or (list? n) 
		       (pair? n))
		   (not (null? n)))
		  ;;si la profondeur des deux sous-arbres est egale
	      (if (eq? (depth (car n)) (depth (cdr n))) 
		  ((pile
		    (- 1 (/ 1 (depth n)))
			;;on imprime deux sous-arbres avec des appels recursifs
		    (cote-a-cote 1/2 (arbre->dessinateur (car n)) (arbre->dessinateur (cdr n))) 
			;;et on imprime un arbre de hauteur 1
		    (superposition (reduction 1/2 1 (ligne (vect -1 -1) (vect 0 1))) 
				   (reduction 1/2 1 (ligne (vect 0 1) (vect 1 -1))))) 0)
		  ;;si les deux sous-arbres n'ont pas la meme profondeur
		  (if (and (not (eq? 0 (depth (car n)))) 
			   (not (eq? 0 (depth (cdr n)))))
		      ((pile (- 1 (/ 1 (depth n)))
			     (cote-a-cote (/ 1 (depth n))
					  ;;on imprime le dessinateur vide sous le sous-arbre dont la profondeur est la moins elevee, pour eviter que ce sous-arbre prenne autant de place verticalement que l'autre sous-arbre de profondeur plus grande
					  (pile (/ (- (depth (cdr n)) (depth (car n))) (depth (cdr n))) vide (arbre->dessinateur (car n))) 
					  (arbre->dessinateur (cdr n)))
			     ;;et on imrpime un arbre de hauteur 1
			     (superposition (reduction (- 1 (/ 1 (depth n))) 1 (ligne (vect -1 -1) (vect 0 1))) 
					    (reduction (/ 1 (depth n)) 1 (ligne (vect 0 1) (vect 1 -1))))) 0)
		      ((pile (- 1 (/ 1 (depth n)))
			     (cote-a-cote (/ 1 (depth n)) (arbre->dessinateur (car n)) (arbre->dessinateur (cdr n)))
			     (superposition (reduction (- 1 (/ 1 (depth n))) 1 (ligne (vect -1 -1) (vect 0 1)))
					    (reduction (/ 1 (depth n)) 1 (ligne (vect 0 1) (vect 1 -1))))) 0)))
		  ;;si la liste ne repond a aucune de des conditions precedentes, on imprime tout simplement le dessinateur vide
	      ((ligne '() '()) 0)))))) 
