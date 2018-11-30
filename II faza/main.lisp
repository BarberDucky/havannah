;;Load Milica
;(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\Havannah - Podeljeni fileovi\\global-variables.lisp")
;(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\Havannah - Podeljeni fileovi\\helpers.cl")
;(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\Havannah - Podeljeni fileovi\\union-rank.cl")
;(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\Havannah - Podeljeni fileovi\\game.cl")
;(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\Havannah - Podeljeni fileovi\\print-board.cl")

;;Load Ana
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\global-variables.lisp")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\helpers.cl")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\union-rank.cl")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\game.cl")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\print-board.cl")


;;********************TEST***************************************
(defvar list)
(setq list (getPossibleStates *board* *human* *matrixDim*))
(defun printAll(lista)
  (cond ((null lista) (return-from printAll))
        (t(printBoard (car lista)) (printAll(cdr lista)))))
(trace printAll)
(untrace printAll)
(printAll list)
;;****************************************************************** 


(havannah)