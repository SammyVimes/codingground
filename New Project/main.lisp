

;unit-testing date comparison
;(print (date_bigger "25.10.2015" "25.11.2015")) ;1
;(print (date_bigger "25.10.2015" "25.10.2015")) ;0
;(print (date_bigger "25.10.2015" "26.10.2015")) ;1
;(print (date_bigger "26.10.2015" "25.10.2015")) ;-1
;(print (date_bigger "25.11.2015" "25.10.2015")) ;-1
;(print (date_bigger "25.10.2016" "25.10.2015")) ;-1
;(print (date_bigger "26.11.2015" "25.10.2016")) ;1
;unit-test date_between
;(print (date_between "25.10.2015" "25.10.2015" "26.10.2015"))
;(print (date_between "24.10.2015" "25.10.2015" "25.11.2015"))

(setq db 'DATABASE)
(create_db db '(
    (ID "Nomer zapisi" T)
    (NAME "Imya chuvaka" NIL)
    (MODEL "Model tachily" NIL)
    (ENGINE "Moshnost' dvizhka" NIL)
    (DATE "Data prodazhi" NIL)
    )
)
;(print (get db 'COLUMNS))
;(print (get db 'ROWS))

(get_col_param (get db 'COLUMNS) 'ID 'nullable)
;(print (is_nullable (get db 'COLUMNS) 'NAME)) ;unit-test fn(is_nullable)
;print (definition (get db 'COLUMNS) 'NAME))   ;unit-test fn(definition)
;(print (user_create_new_row db))
;(print (get db 'ROWS))
(print "Lets go to next row")
;(print (user_create_new_row db))
(create_new_row db '("1" "MR BLACK" "HUMMER" "150" "24.10.2015"))
(create_new_row db '("2" "MR BROWN" "HUMMER" "150" "24.10.2015"))
(create_new_row db '("3" "MR BLACK" "HUMMER" "150" "24.10.2015"))
(create_new_row db '("4" "MR BLACK" "HUMMER" "150" "24.10.2015"))
;(print (select db '((ID (LAMBDA (val) (string= val "2"))) (MODEL (LAMBDA (val) (string= val "KOPEYKA")))))) ;unit-test for simple select
;(print (get db 'ROWS))
;(print (get_value (get db 'COLUMNS) 'NAME (nth 0 (get db 'ROWS)))) ;checking get_value
(print (select_equals db '((ID "2") (MODEL "KOPEYKA")))) ;unit-test for select-equals
(print "Selecting by date")
;(print (select db 
;            '(
;                (DATE (LAMBDA (val) (date_between val "25.10.2015" "25.11.2015")))
;            )
;        )
;)
;(print (select_date db "25.10.2015" "25.11.2015"))
(print (select_by_date_and_model db "KOPEYKA" "25.10.2015" "25.11.2015"))
