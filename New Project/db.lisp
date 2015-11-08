(DEFUN PUT (sym prop val) (setf (get sym prop) val))
(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))
     

(defun create_columns (col_list)
     (let ((DB_COLUMNS '())) 
	     (loop
	       (setq cur_element (CAR col_list))
	       (setq col_name (CAR cur_element))
	       (put col_name 'definition (nth 1 cur_element))
	       (put col_name 'nullable (nth 2 cur_element))
               (setq DB_COLUMNS (append DB_COLUMNS (list col_name)))
	       (setq col_list (CDR col_list))
	       (if (NULL col_list) (return DB_COLUMNS))
	     )
     )
)

(defun get_col_param (columns col_name param) 
     (if (NULL columns) 
        NIL
	    (if (eql (car columns) col_name) (get (car columns) param) (get_col_param (cdr columns) col_name param))
	  )
)

(defun is_nullable (columns column_name) 
     (if (get_col_param columns column_name 'nullable) T NIL)
)

(defun definition (columns column_name) 
     (get_col_param columns column_name 'definition)
)

;user input

(defun read_user_input () (read-line))


(defun read_user_line (col_name definition nullable)
    (write-line (concatenate 'string "Enter value for " (string col_name)))
    (write-line (concatenate 'string "This value means " (string definition)))
    (loop
        (let ((userline (read_user_input)))
            (if (= (length userline) 0)
                (if nullable (return userline) (write-line "Cant be empty! Try again..."))
                (return userline)
            )
        )
    )
)

(defun user_create_new_row (db)
    (let ((column_names (get db 'COLUMNS)) (columns (get db 'COLUMNS)) (result '()))
        (loop
            (if (NULL columns) (return (put db 'ROWS (nconc (get db 'ROWS) (list result)))))
            (let ((cur_col (car columns)))
                (setq result (append result 
                                    (list (read_user_line 
                                        cur_col 
                                        (definition column_names cur_col) 
                                        (is_nullable column_names cur_col)
                                    ))
                              )
                )
                (setq columns (cdr columns))
            )
        )
    )
)

(defun create_new_row (db lst)
    (put db 'ROWS (nconc (get db 'ROWS) (list lst)))
)

(defun create_db (db col_list)
    (put db 'COLUMNS (create_columns col_list))
    (put db 'ROWS ())
)

(defun get_value (db col_name row)
    (let ((columns (get db' COLUMNS)) (i 0))
        (loop
            (if (NULL columns) (return NIL))
            (if (eql (car columns) col_name)
                (return (nth i row)))
            (setq columns (cdr columns))
            (setq i (+ i 1))
        )
    )
)

(defun predicate (nm fn)
    (let (lst)
        (setq lst (append lst (list nm)))
        (setq lst (append lst (list fn)))
        (if T lst)
    )       
)

(defun select_delete (db pairs)
    (let ((result ()) (allrows (get db 'ROWS)) (rows (get db 'ROWS)) (columns (get db' COLUMNS)) row)
        (loop
            (if (NULL rows) (return result))
            (setq row (car rows))
            (let ((ppairs pairs) (matches T) colname filter value)
                (loop
                    (if (NULL ppairs)
                        (if matches 
                            (return NIL)
                            (return (setq result (append result (list row))))
                        )
                    )
                    (setq colname (nth 0 (car ppairs)))
                    (setq filter (nth 1 (car ppairs)))
                    (setq value (get_value db colname row))
                    (if (funcall (eval filter) value) T (setq matches NIL))
                    (setq ppairs (cdr ppairs))
                )
            )
            (setq rows (cdr rows))
        )
        (put db 'ROWS result)
    )
)

(defun select (db pairs)
    (let ((result ()) (rows (get db 'ROWS)) (columns (get db' COLUMNS)) row)
        (loop
            (if (NULL rows) (return result))
            (setq row (car rows))
            (let ((ppairs pairs) (matches T) colname filter value)
                (loop
                    (if (NULL ppairs)
                        (if matches 
                            (return (setq result (append result (list row))))
                            (return NIL)
                        )
                    )
                    (setq colname (nth 0 (car ppairs)))
                    (setq filter (nth 1 (car ppairs)))
                    (setq value (get_value db colname row))
                    (if (funcall (eval filter) value) T (setq matches NIL))
                    (setq ppairs (cdr ppairs))
                )
            )
            (setq rows (cdr rows))
        )
    )
)

(defun select_equals (db pairs)
    (let ((ppairs pairs) lst cur tmp tmp_val)
        (loop
            (if (NULL ppairs) (return (select db lst)))
            (setq cur (car ppairs))
            (setq tmp (predicate (nth 0 cur) ((LAMBDA (tmp_val) (LAMBDA (val) (string= val tmp_val))) (nth 1 cur))))
            (setq lst (append lst (list tmp)))
            (setq ppairs (cdr ppairs))
        )
    )
)

(defun select_date (db date1 date2)
    (let (lst)
        (setq lst (predicate 'DATE ((LAMBDA (date1 date2) (LAMBDA (val) (date_between val date1 date2))) date1 date2)))
        (select db (list lst))
    )
)

(defun select_by_date_and_model (db model date1 date2)
    (let (lst)
        (setq lst (list (predicate 'DATE ((LAMBDA (date1 date2) (LAMBDA (val) (date_between val date1 date2))) date1 date2))))
        (NCONC lst (list (predicate 'MODEL ((LAMBDA (model) (LAMBDA (val) (string= val model))) model))))
        (select db lst)
    )
)

(defun date_bigger (l r)
    (let ((left (split-str l ".")) (right (split-str r ".")) (i 0) (val 0) curL curR)
        (loop
            (if (= i 3) (return val))
            (setq curL (parse-integer (nth i left)))
            (setq curR (parse-integer (nth i right)))
            (cond
                ((> curL curR) (setq val -1))
                ((< curL curR) (setq val 1))
            )
            (setq i (+ i 1))
        )
    )
)

(defun date_between (cur l r)
    (if (or (= (date_bigger l cur) 1) (= (date_bigger l cur) 0))
        (if (or (= (date_bigger r cur) -1) (= (date_bigger r cur) 0)) T NIL)
        NIL
    )
)

(defun integer_between (cur l r)
    (if (or (= l cur) (> cur l))
        (if (or (= r cur) (> r cur)) T NIL)
        NIL
    )
)