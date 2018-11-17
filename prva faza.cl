;;definisanje promenljivih

(defconstant *praznoPolje* '-)
(defconstant *nevalidno* '0)

;; n je dimenzija table
(defvar *n* )
;; dimenzija matrice
(defvar *dimenzijaMatrice*)
;; tabla
(defvar *tabla*)
;;///////////////////////////////////////////////////////////////////

;;Generisanje uzastopnih istih elemenata
(defun generisiClanove (element broj)
  (cond ( (zerop broj) '() )
        ( t (cons element (generisiClanove element (- broj 1))) )
        ))

;;Generisanje jednog reda u matrici
(defun generisiRed (prvi brojPrvi drugi brojDrugi)
  (append (generisiClanove prvi brojPrvi) (generisiClanove drugi brojDrugi)))

;;Generisanje cele matrice
(defun generisi (dim prviBroj drugiBroj donjiDeo )
  (cond 
   ( (and donjiDeo (= prviBroj *n*)) '())
   ( (= dim (- prviBroj 1)) (generisi dim (+ drugiBroj 2) (- prviBroj 2) t ))
   ( (null donjiDeo)  (cons (generisiRed  *praznoPolje* prviBroj *nevalidno* drugiBroj) 
                      (generisi dim (+ 1 prviBroj) (- drugiBroj 1 ) donjiDeo)))
    (t(cons (generisiRed *nevalidno*  prviBroj *praznoPolje*  drugiBroj) 
                      (generisi dim (+ 1 prviBroj) (- drugiBroj 1 ) donjiDeo)))
   ))

;;Ucitavanje dimenzija i postavljanje table 
(defun postaviDimenzije ()
  (format t "~%Unesite dimenziju table: ")
  (let ((input (read)))
    (cond ( (or (not (numberp input)) (< input 1) (> input 10)) (format t "~%Pogresan unos dimenzije!") (postaviDimenzije) )
          ( t 
           (setq *n* input) 
           (setq *dimenzijaMatrice* (- (* 2 *n*) 1))
           (setq *tabla* (generisi *dimenzijaMatrice* *n* (- *n* 1) '()))
           ))))
;;STAMPANJE MATRICE ZA SAD
(defun igra ()
  (postaviDimenzije)
  (format t "~%~{~{~a~^ ~}~%~}" *tabla*)
  )

(defun stampajRed (lista dim indexReda)
  (cond 
   ((= dim '-1) (format t "~a " (code-char (+ 65 indexReda))) (stampajRed lista '0 indexReda))
   ((= dim *dimenzijaMatrice*) (format t "~%"))
        ((equalp (car lista) '0) (format t " ") (stampajRed (cdr lista) (+ dim 1) indexReda))
        (t(format t "~a " (car lista)) (stampajRed (cdr lista) (+ dim 1) indexReda ))))

(defun stampajTablu (tabla red)
  (cond ((null tabla) (format t "~%"))
        ((>= red *n*) (stampajRed (car tabla) '-1 red) (stampajTablu (cdr tabla) (+ red 1)))
        (t(stampajRed(append (member '0 (car tabla)) (car tabla)) '-1 red) (stampajTablu (cdr tabla) (+ red 1)))))

;; igranje poteza
(defun postavi (el i j lista)
  (cond 
   ((null lista) '())
        ((> i '0) (cons (car lista) (postavi el (- i 1) j (cdr lista))))
        ((equalp i '0) (cons (postavi el (- i 1) j (car lista)) (cdr lista)))
        ((equalp j '0) (cons el (postavi el i (- j 1) (cdr lista))))
        (t(cons (car lista) (postavi el (- i 1) (- j 1) (cdr lista))))))

(defun odigrajPotez (igrac)
  (format t "~%Unesite slovo za red i broj za kolonu: ")
  (let* ((slovo (read-char)) (i (- (char-code slovo) 65)) (j (read)))
    (cond ((not (equalp (nth j (nth i *tabla*)) '-)) (format t "~%Pogresan unos!") (odigrajPotez igrac))
          (t (setq *tabla* (postavi igrac i j *tabla*))))
          ))
(igra)
(odigrajPotez 'X)
(stampajTablu *tabla* '0)
