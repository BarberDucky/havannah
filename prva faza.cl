;;definisanje promenljivih ****************************************************************************************************************************************************

(defconstant *praznoPolje* '-)
(defconstant *nevalidnoPolje* '0)
(defvar *n* )                                ;; n je dimenzija table
(defvar *dimenzijaMatrice*)                  ;; dimenzija matrice/table
(defvar *tabla*)                             ;; promenljiva koja cuva tablu

;;Generisanje matrice *********************************************************************************************************************************************************

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
   ( (null donjiDeo)  (cons (generisiRed  *praznoPolje* prviBroj *nevalidnoPolje* drugiBroj) 
                      (generisi dim (+ 1 prviBroj) (- drugiBroj 1 ) donjiDeo)))
    (t(cons (generisiRed *nevalidnoPolje*  prviBroj *praznoPolje*  drugiBroj) 
                      (generisi dim (+ 1 prviBroj) (- drugiBroj 1 ) donjiDeo)))
   ))

;;Ucitavanje dimenzija i pocetak igre *****************************************************************************************************************************************

;;Ucitavanje dimenzija i postavljanje table 
(defun postaviDimenzije ()
  (format t "~%Unesite dimenziju table: ")
  (let ((input (read)))
    (cond 
     ( (or (not (numberp input)) (< input 1) (> input 10)) (format t "~%Pogresan unos dimenzije!") (postaviDimenzije) )
     ( t 
      (setq *n* input) 
      (setq *dimenzijaMatrice* (- (* 2 *n*) 1))
      (setq *tabla* (generisi *dimenzijaMatrice* *n* (- *n* 1) '()))
      ))))

(defun igra ()
  (postaviDimenzije)
  (format t "~%~{~{~a~^ ~}~%~}" *tabla*))

;;Stampanje table ***************************************************************************************************************************************************************

;;Stampa newline i prvi red matrice
(defun stampajPrviRed (brojac)                                                                           
  (cond
   ((equalp brojac -1) (format t "~%") (stampajPrviRed 0))                                                          ;stampanje newline za celu matricu
   ((equalp brojac (+ '2 *dimenzijaMatrice*)) '())                                                                  ;kraj                                                
   ((< brojac (+ *n* '1)) (format t " ") (stampajPrviRed (+ brojac '1)))                                            ;stampanje razmaka - n+1 puta                                  
   (t (format t "~a " (- brojac *n* 1)) (stampajPrviRed (+ brojac '1)))))                                           ;stampanje brojeva - n puta

;;Stampa newline i svaki red matrice osim prvog 
(defun stampajRed (lista brojac indexReda)
  (cond 
   ((= brojac '-1) (format t "~%~a " (code-char (+ 65 indexReda))) (stampajRed lista '0 indexReda))                 ;prvi el = newline + slovo
   ((= brojac *dimenzijaMatrice*) (if (< indexReda (- *n* 1)) (format t "~a" (+ indexReda *n*)) '()))               ;posl el broj se stampa samo za gornju 1/2 matrice
   ((equalp (car lista) '0) (format t " ") (stampajRed (cdr lista) (+ brojac 1) indexReda))                         ;nule se pretvaraju u blanko
   (t (format t "~a " (car lista)) (stampajRed (cdr lista) (+ brojac 1) indexReda ))))                              ;elementi se stampaju kao element + blanko

;;Stampa celu tablu
(defun stampajTablu (tabla red)
  (cond 
   ((null tabla) (format t "~%"))                                                                                   ;kraj
   ((= red -1) (stampajPrviRed -1) (stampajTablu tabla 0))                                                          ;newline + prvi red
   ((>= red *n*) (stampajRed (car tabla) '-1 red) (stampajTablu (cdr tabla) (+ red 1)))                             ;za donju 1/2 table saljemo direkt        
   (t(stampajRed(append (member '0 (car tabla)) (car tabla)) '-1 red) (stampajTablu (cdr tabla) (+ red 1)))))       ;za gornju 1/2 table premestamo nule ispred

;; Igranje poteza **************************************************************************************************************************************************************

(defun postavi (el i j lista)
  (cond 
   ((null lista) '())
        ((> i '0) (cons (car lista) (postavi el (- i 1) j (cdr lista))))
        ((equalp i '0) (cons (postavi el (- i 1) j (car lista)) (cdr lista)))
        ((equalp j '0) (cons el (postavi el i (- j 1) (cdr lista))))
        (t(cons (car lista) (postavi el (- i 1) (- j 1) (cdr lista))))))

(defun odigrajPotez (igrac)
  (format t "~%Unesite slovo za red pa enter i broj za kolonu pa enter: ")
  (let* ((slovo (read-char)) (i (- (char-code slovo) 65)) (j (read)))
    (cond ((not (equalp (nth j (nth i *tabla*)) '-)) (format t "~%Pogresan unos!") (odigrajPotez igrac))
          (t (setq *tabla* (postavi igrac i j *tabla*))))
          ))

;;Pozivi funkcija ***************************************************************************************************************************************************************

(igra)
(odigrajPotez 'X)
(stampajTablu *tabla* '-1)







