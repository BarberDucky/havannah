;; Board printout *********************************************************************************************************************************************************************

(defun printFirstRow (count)                                                                           
  (cond
   ((equalp count -1) (format t "~%") (printFirstRow 0))                                                                ;print newline for the entire matrix
   ((equalp count (+ '2 *matrixDim*)) '())                                                                              ;end                                                
   ((< count (+ *n* '1)) (format t " ") (printFirstRow (+ count '1)))                                                   ;print space - n+1 times                                  
   (t (format t "~a " (- count *n* 1)) (printFirstRow (+ count '1)))))   

(defun printBoard (board)
  (format t "~%")
  (printFirstRow '-1) 
  (format t "~%")
  (progn
    (dotimes (i (- *n* 1))
      (format t "~a " (code-char (+ 65 i)))
      (dotimes (zeroNum (- *n* 1 i))
        (format t " "))
      (dotimes (j (+ *n* i))
        (format t "~a " (cell-value (aref board i j)))
        )
      (format t "~a~%" (+ *n* i))
      )
    (dotimes (i *n*) 
      (format t "~a " (code-char (+ 65 i (- *n* 1))))
      (dotimes (j *matrixDim*) 
      (cond ( (equalp (aref board (+ i (- *n* 1)) j) *invalidField*) (format t " ") )
            ( t (format t "~a " (cell-value (aref board (+ i (- *n* 1)) j))) )
            )
      )
    (format t "~%")
      )))

