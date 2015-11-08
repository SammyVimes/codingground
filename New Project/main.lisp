(load "db.lisp")

(defun f21blen (cars soldcars manufacturer date1 date2)
    (length (f21b cars soldcars manufacturer date1 date2))
)

(defun f21b (cars soldcars manufacturer date1 date2)
    (let ((match (select_equals cars (list (append (list 'MANUFACTURER) (list manufacturer))))) result CAR_ID tmpres)
        (loop
            (if (NULL match) (return result))
            (setq CAR_ID (get_value cars 'ID (car match)))
            (setq tmpres
                (select soldcars (append (list
                                            (predicate 'ID_EXTERNAL ((LAMBDA (CAR_ID) 
                                                                            (LAMBDA (VAL) (string= val CAR_ID)) 
                                                                      ) CAR_ID
                                                                    )
                                            )
                                         )
                                         (list
                                            (predicate 'BUY_DATE ((LAMBDA (DATE1 DATE2) 
                                                                            (LAMBDA (VAL) (date_between val DATE1 DATE2)) 
                                                                      ) date1 DATE2
                                                                  )
                                            )
                                         )
                                 )
                )
            )
            (setq result (append result tmpres))
            (setq match (cdr match))
        )
    )
)

(defun f22a (cars sitsmin sitsmax enginemin enginemax)
    (setq sitsmin (parse-integer sitsmin))
    (setq sitsmax (parse-integer sitsmax))
    (setq enginemin (parse-integer enginemin))
    (setq enginemax (parse-integer enginemax))
    (setq tmpres
        (select cars (append (list
                                    (predicate 'SITS_COUNT ((LAMBDA (sitsmin sitsmax) 
                                                                    (LAMBDA (VAL) (integer_between (parse-integer  VAL) sitsmin sitsmax)) 
                                                              ) sitsmin sitsmax
                                                            )
                                    )
                                 )
                                 (list
                                    (predicate 'ENGINE_POWER ((LAMBDA (enginemin enginemax) 
                                                                    (LAMBDA (VAL) (integer_between (parse-integer  VAL) enginemin enginemax)) 
                                                              ) enginemin enginemax
                                                          )
                                    )
                                 )
                         )
        )
    )
)

(defun f22b (cars soldcars date1 date2)
    (let ((match (select cars NIL)) result CAR_ID tmpres)
        (loop
            (if (NULL match) (return result))
            (setq CAR_ID (get_value cars 'ID (car match)))
            (setq MARKA (get_value cars 'MANUFACTURER (car match)))
            (setq tmpres
                (select soldcars (append (list
                                            (predicate 'ID_EXTERNAL ((LAMBDA (CAR_ID) 
                                                                            (LAMBDA (VAL) (string= val CAR_ID)) 
                                                                      ) CAR_ID
                                                                    )
                                            )
                                         )
                                         (list
                                            (predicate 'BUY_DATE ((LAMBDA (DATE1 DATE2) 
                                                                            (LAMBDA (VAL) (date_between val DATE1 DATE2)) 
                                                                      ) date1 DATE2
                                                                  )
                                            )
                                         )
                                 )
                )
            )
            (setq CAR_ID (get_value cars 'ID (car match)))
            (setq result (append result (list (list MARKA (length tmpres)))))
            (setq match (cdr match))
        )
    )
)

(defun f23a (cars country)
    (let ((match (select_equals cars (list (append (list 'COUNTRY) (list country))))) result)
        (loop
            (if (NULL match) (return result))
            (setq MARKA (get_value cars 'MANUFACTURER (car match)))
            (setq POWER (get_value cars 'ENGINE_POWER (car match)))
            (setq result (append result (list (list MARKA POWER))))
            (setq match (cdr match))
        )
    )
)

(defun f23b (cars soldcars enginenum)
    (let ((pr (list (append (list 'ENGINE_NUMBER) (list enginenum)))) match result CAR_ID tmpres)
        (setq match (select_equals cars pr))
        (loop
            (if (NULL match) (return result))
            (setq CAR_ID (get_value cars 'ID (car match)))
            (select_delete soldcars (list (predicate 'ID_EXTERNAL 
                                        ((LAMBDA (CAR_ID) 
                                            (LAMBDA (VAL) (string= val CAR_ID))) CAR_ID)
                                   )
                              )
            )
            (select_delete cars (list (predicate 'ID 
                                        ((LAMBDA (CAR_ID) 
                                            (LAMBDA (VAL) (string= val CAR_ID))) CAR_ID)
                                   )
                              )
            )
            (setq result (append result tmpres))
            (setq match (cdr match))
        )
    )
)



(setq TCARS 'CARS)
(setq TCARSSOLD 'CARS_SOLD)
(load_db TCARS "columns_cars.txt" "cars.txt")
(load_db TCARSSOLD "columns_sold_cars.txt" "sold_cars.txt")
;(create_db TCARS '(
;    (ID "ID mashiny" NIL)
;    (MANUFACTURER "Manufactorer" T)
;    (ENGINE_POWER "Moshnost' dvigatelya" T)
;    (ENGINE_NUMBER "Nomer dvigatelya" T)
;    (DATE "Data vipuska" T)
;    (COUNTRY "Strana postavki" T)
;    (BODY_TYPE "Tip kuzova" T)
;    (BODY_COLOR "Cvet kuzova" T)
;    (SITS_COUNT "4islo passaghirskih mest" T)
;    (PRICE "Cena" T)
;    )
;)
;(create_new_row TCARS '("1" "Mitsubishi Gallardo NFS Edition" "150" "R123" "24.09.2011" "Yaponia" "Hatchback" "Dark black" "5" "2500000"))
;(create_new_row TCARS '("2" "Mitsubishy Lancer Huevolution" "155" "R125" "4.5.2015" "Yaponia" "Pickup" "White" "3" "1750000"))
;(create_new_row TCARS '("3" "Lada 2110" "250" "ART1" "17.10.1892" "Russia" "Sedan" "Red" "13" "100000"))
;(create_new_row TCARS '("4" "Lada Priora" "200" "ARG65" "5.8.1965" "Russia" "Sedan" "White" "3" "200000"))
;(create_db TCARSSOLD '(
;    (ID "ID prodazhi" NIL)
;    (ID_EXTERNAL "ID mashiny" NIL)
;    (BUYER_NAME "Imya pokupatelya" NIL)
;    (BUY_DATE "Data prodazhi" NIL)
;    )
;)
;(create_new_row TCARSSOLD '("1" "3" "Vasyan" "8.11.1978"))
;(create_new_row TCARSSOLD '("2" "2" "Edos" "5.4.2014"))
;(create_new_row TCARSSOLD '("3" "1" "Alexandro" "15.10.2014"))
;(create_new_row TCARSSOLD '("4" "4" "Alexandro" "25.12.2014"))
;(create_new_row TCARSSOLD '("5" "4" "Alexandro" "7.2.2015"))
;(create_new_row TCARSSOLD '("6" "3" "Jesper" "17.11.2003"))

(print (f21b TCARS TCARSSOLD "Lada Priora" "1.1.1965" "1.1.2016"))
(print (f21blen TCARS TCARSSOLD "Lada Priora" "1.1.1965" "1.1.2016"))
(print (f22a TCARS "3" "15" "152" "251"))
(print (f22a TCARS "3" "10" "152" "251"))
(print (f22a TCARS "3" "10" "152" "201"))
(print (f22b TCARS TCARSSOLD "1.1.1965" "1.1.2016"))
(print (f23a TCARS "Yaponia"))
(write-line "")
(write-line "Testing deletion")
(print (select TCARS NIL))
(print (select TCARSSOLD NIL))
(f23b TCARS TCARSSOLD "ART1")
(print (select TCARS NIL))
(print (select TCARSSOLD NIL))

;(print (get db 'COLUMNS))
;(print (get db 'ROWS))

;(get_col_param (get db 'COLUMNS) 'ID 'nullable)
;(print (is_nullable (get db 'COLUMNS) 'NAME)) ;unit-test fn(is_nullable)
;print (definition (get db 'COLUMNS) 'NAME))   ;unit-test fn(definition)
;(print (user_create_new_row db))
;(print (get db 'ROWS))
;(print "Lets go to next row")
;(print (user_create_new_row db))
;(print (select db '((ID (LAMBDA (val) (string= val "2"))) (MODEL (LAMBDA (val) (string= val "KOPEYKA")))))) ;unit-test for simple select
;(print (select db '((COUNTRY (LAMBDA (val) (string= val "RUSSIA")))))) ;unit-test for simple select
;(print (get db 'ROWS))
;(print (get_value (get db 'COLUMNS) 'NAME (nth 0 (get db 'ROWS)))) ;checking get_value
;(print (select_equals db '((ID "2") (MODEL "KOPEYKA")))) ;unit-test for select-equals
;(print "Selecting by date")
;(print (select db 
;            '(
;                (DATE (LAMBDA (val) (date_between val "25.10.2015" "25.11.2015")))
;            )
;        )
;)
;(print (select_date db "25.10.2015" "25.11.2015"))
;(print (select_by_date_and_model db "KOPEYKA" "25.10.2015" "25.11.2015"))