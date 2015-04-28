;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


;; function defns 
(defun atom? (x) (not (listp x)))

(add-text-properties 1 906
		     '(comment t face highlig ht))


(defun lat? (l)
  (cond
   ((null l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)   
  ))

(defun member? (a lat)
  (cond
   ((null lat) nil)
   ((eq a (car lat)) t)
   ((member? a (cdr lat)))
   )
  )

(defun conlat (lat)
  (cond
   ((null lat) ())
   (t
    (cons (car (car lat))
	  (conlat (cdr lat))
	  )
    )
   )
  )

(defun insertR (new old lat)
  (cond
   ((null lat) '())
   ((eq (car lat) old)
    (cons old 
	  (cons new (cdr lat))))
   (t
    (cons (car lat)
	  (insertR new old (cdr lat))))
   ))


(defun multirember (a lat)
  (cond
   ((null lat) '())
   ((eq a (car lat)) (multirember a (cdr lat)))
   ((cons
     (car lat)
     (multirember a (cdr lat)))
	  )
   ))


(defun multiinsertR (new old lat)
  (cond
   ((null lat) '())
   ((eq old (car lat))
    (cons old
	  (cons new
		(multiinsertR new old (cdr lat))
		)))
    (t
     (cons (car lat)
	   (multiinsertR new old (cdr lat)
			 )))))


(defun +s (a b)
  (cond
   ((zerop b) a)
   (t (+s (1+ a) (1- b)))
   )
  )


(+s 2 4)

;; tests and calls
(multiinsertR 'e 'b '(e))

(insertR 'd 'c '(a b c e f g ))

(multirember 'b '(a b b c))

(member? 'a '(c a d))
(lat? '(a b))
(atom?
 (car
  (cdr '(a (b c) d e))))

(setq a '(harry mary))
(eq a '(harry mary))
(eq "harry" "harry")
(eq 5 5)

(setq l '(b b w n j b))
(eq (car l) (car (cdr l)))

(cons 'a '(b c))

(1+ 54)
(zerop 9)
