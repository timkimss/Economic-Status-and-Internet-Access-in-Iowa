#lang racket
(require csc151)
(require plot)

;;;-----------------
;;;Instructions

#|
Enter plotted-income-county-code to generate a plot of income brackets by $5000 of each county of Iowa represented as the FIPS code.
Enter (codes-bracket-plot XXXXX-counties) to generate a plot of the number of each of the 14 tech codes of all counties of Iowa in the income bracket.
   The XXXXX should be replaced by 30000, 35000, 40000, 45000, 50000, 55000, or 60000
Enter all-brackets to generate a stacked hisogram of all the tallied tech codes in all of the counties of Iowa separated by income brackets.
|#

;;;-----------------
;;;Default


;;; Procedure:
;;;   select-columns
;;; Parameters:
;;;   table, a list of lists
;;;   column-indices, a list
;;; Purpose:
;;;   To produce a table with the requested column-indices
;;; Produces:
;;;   columns, a table
;;; Preconditions:
;;;   column-indices must be a list of nonnegative integers that can serve as viable indices.
;;;   inner lists inside table must have the same length
;;; Postconditions:
;;;   (= (length columns) (length table))
;;;   length of an a list inside columns is equal to length of column-indices
(define select-columns
  (lambda (table column-indices)
    (cond
      [(null? table)
       null]
      [((negate null?) table)
       (cons (map (section list-ref (car table) <>) column-indices)
             (select-columns (cdr table) column-indices))])))

;;; Procedure:
;;;   make-pair-accessor
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   produces a pair accessor procedure from a string that has a Scheme-style name
;;; Produces:
;;;   new-proc, a procedure
;;; Preconditions:
;;;   a string with the first character "c", last character "r", and some combination of "a" and "d" in between  
;;; Postconditions:
;;;   For every list lst (new-proc lst) returns a value from lst or a list containing values from lst.
;;;   new-proc is a procedure that can be written with some combination of car and cdr
(define make-pair-accessor
  (lambda (name)
    (lambda (input)
      (let [(lst (string->list name))]
        (let kernel [(newlst (reverse (drop (take lst (- (length lst) 1)) 1)))
                     (result input)]
          (cond
            [(null? newlst) result]
            [(char-ci=? (car newlst) #\a) (kernel (cdr newlst) (car result))]
            [else (kernel (cdr newlst) (cdr result))]))))))



;;;-------------------
;;;Income Data



(define O-CAINC1
  (read-csv-file "~/Desktop/CSC151/CAINC1_IA_2001_2017.csv")) ;original CAINC1 file
;relevant information in this file includes GEOFIPS code, Name associated with the code, and per capita income of 2017.

;(read-csv-file "C:/Users/Timothy Kim/Desktop/CSC151/CAINC1_IA_2001_2017.csv")) 

(define clean-O-CAINC1
  (reverse (drop (reverse (drop O-CAINC1 1)) 3))) ;dropped irrelevent lines 



(define my-caddddr
  (make-pair-accessor "caddddr")) ;takes value at 4th index in a list


;;; Procedure:
;;;   line-code-line-code<?
;;; Parameters:
;;;   CAIN1, a list
;;;   CAIN2, a list
;;; Purpose:
;;;   Compares two linecodes based on numeric value.
;;; Produces:
;;;   larger? a Boolean value
;;; Preconditions:
;;;   CAIN1 and CAIN2 are lists that have the linecode as the 5th index
;;; Postconditions:
;;;   If the linecode of CAIN1 is greater than the linecode of CAIN2, larger? is #t. Otherwise, it is #f.
(define line-code-line-code<?
  (lambda (CAIN1 CAIN2)
    (> (my-caddddr CAIN1) (my-caddddr CAIN2))))


(define finished-CAINC1
  (take (sort clean-O-CAINC1 line-code-line-code<?) (/ (length clean-O-CAINC1) 3)))
;took out lines that included total population and total income (only per capita income is relevant)


(define reduced-GEO-FIPS
  (map (o string->number list->string (section take <> 3) (section drop <> 4) string->list car) finished-CAINC1))
;a list of all the GEO-FIPS codes without the starting two numbers in the code. Those two numbers are 19, indicating
;that it is a county in the state of Iowa. They are irrelevant as we are only looking at Iowan counties.

(define county-name
  (map cadr finished-CAINC1))
;a list of all Iowan county names

(define index-24 (make-pair-accessor "caddddddddddddddddddddddddr"))
;takes value at 24th index in a list.
;this is necessary as the 2017 per capita income is in the 24th index of each list in the CAINC1 table

(define per-capita-income
  (map (o string->number list->string reverse (section drop <> 1) reverse string->list index-24) finished-CAINC1))
;the per-capita-income was given as strings in lists so this converted it into numbers

(define final-CAINC1
  (map list reduced-GEO-FIPS county-name per-capita-income))
;table containing the reduced FIPS code, county name, and per capita income

(define bracket-CAINC1
  (map list reduced-GEO-FIPS county-name (map (o (section * <> 5000) round (section / <> 5000)) per-capita-income)))
;same table as final-CAINC1 except the per capita income is now in brackets of $5000


;;;---------------------------------------------------------------
;;;Broadband Data


(define OFCC
  (read-csv-file "~/Desktop/CSC151/Fixed_Broadband_Deployment_Data_June_2017_Status_V1_short.csv"))
;original FCC, Fixed Broadband Deployment Data
;(read-csv-file "C:/Users/Timothy Kim/Desktop/CSC151/Fixed_Broadband_Deployment_Data_June_2017_Status_V1_short.csv"))

(define clean-OFCC
  (drop OFCC 1))
;dropped irrelevant lists in OFCC

(define final-FCC
  (select-columns clean-OFCC (list 9 10 15 16)))
;took the relevant columns in clean-OFCC

(define short-FCC
  (take final-FCC 300))

#|
The tech codes displaying which internet connections were used

10 = Asymmetric xDSL
11 = ADSL2, ADSL2+
12 = VDSL
20 = Symmetric xDSL*
30 = Other Copper Wireline (all copper-wire based technologies
other than xDSL; Ethernet over copper and T-1 are examples)
40 = Cable Modem other than DOCSIS 1, 1.1, 2.0, 3.0 or 3.1
41 = Cable Modem – DOCSIS 1, 1.1 or 2.0
42 = Cable Modem – DOCSIS 3.0
43 = Cable Modem – DOCSIS 3.1
50 = Optical Carrier / Fiber to the end user (Fiber to the home or
business end user, does not include “fiber to the curb”)
60 = Satellite†
70 = Terrestrial Fixed Wireless
90 = Electric Power Line
0 = All Other

|#

(define FIPS-code
  (map (o inexact->exact floor (section - <> 19000) (section / <> 10000000000) car) short-FCC))
;cleaned the FIPS-code so that they are one to three digit numbers that are consistent with the FIPS-code in the CAINC1 form.

(define tech-code
  (map cadr short-FCC))
;all the found tech codes used in the iowa counties.

(define downstream-speed
  (map caddr short-FCC))
;planned to be used but was not.

(define upstream-speed
  (map (o string->number list->string reverse (section drop <> 1) reverse string->list cadddr) short-FCC))
;planned to be used but was not.

(define with-county-code
  (map list FIPS-code tech-code))
;FIPS codes with the respective tech codes that it has

(define county-code-bracket-income
  (map list reduced-GEO-FIPS (map (o (section * <> 5000) round (section / <> 5000)) per-capita-income)))
;FIPS codes with the respective income bracket that the county is in

;The next few definitions gives a list of all the FIPS codes of the counties in the respective income brackets.
(define 30000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 30000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

(define 35000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 30000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

(define 40000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 40000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

(define 45000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 45000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

(define 50000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 50000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

(define 55000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 55000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

(define 60000-counties
  (let kernel [(old county-code-bracket-income)
               (new null)]
    (cond
      [(null? old)
       (reverse new)]
      [(= (cadr (car old)) 60000)
       (kernel (cdr old) (cons (car (car old)) new))]
      [else
       (kernel (cdr old) new)])))

  
;;; Procedure:
;;;   county-code-county-code<?
;;; Parameters:
;;;   FCC1, a list
;;;   FCC2, a list
;;; Purpose:
;;;   Compare two county codes based off of numeric value
;;; Produces:
;;;   smaller?, a Boolean value
;;; Preconditions:
;;;   FCC1 and FCC2 are lists that have the county code as the 0th index
;;; Postconditions:
;;;   * If the county code of FCC1 is smaller than the county code of FCC2, smaller? is #t. Otherwise, smaller? is #f.
(define county-code-county-code<?
  (lambda (FCC1 FCC2)
    (< (car FCC1) (car FCC2))))

(define placeholders
  (list (list 0 10)
        (list 0 11)
        (list 0 12)
        (list 0 20)
        (list 0 30)
        (list 0 40)
        (list 0 41)
        (list 0 42)
        (list 0 43)
        (list 0 50)
        (list 0 60)
        (list 0 70)
        (list 0 90)
        (list 0 0)))
;To make sure the output has the same order as the list of tech codes given.

(define sorted-by-county-code
  (sort with-county-code county-code-county-code<?))
;procedure that given a FIPS-code, returns a list of all the tech-codes. Sorted by increasing FIPS-code



(define tally-tech-code
  (let kernel [(lst sorted-by-county-code)
               (current-lst null)
               (final-lst null)
               (count 0)]
    (cond
      [(null? lst)
       final-lst]
      [(not (= (car (car lst)) count))
       (if (null? current-lst)
           (kernel lst null final-lst (+ count 1))
           (kernel lst null (append final-lst (list count (tally-all current-lst))) (+ count 1)))]
      [else
       (kernel (cdr lst) (append current-lst (cdr (car lst))) final-lst count)])))
;tally-tech-code returns the FIPS code and all the tech codes associated with that FIPS code and tallied.


(define all-together
  (let kernel [(lst tally-tech-code)
               (ilst bracket-CAINC1)
               (final null)]
    (cond
      [(null? ilst)
       final]
      [else
       (if (= -1 (index-of (car (car ilst)) lst))
           (kernel lst (cdr ilst) (append final (list (append (car ilst) null))))
           (kernel lst (cdr ilst) (append final (list (append (car ilst) (list (list-ref lst (+ (index-of (car (car ilst)) lst) 1))))))))])))
;all-together returns a table of the FIPS code, county name, the income bracket, and the tallied tech codes.
;More can be done with all-together.

(define plotted-income-county-code
  (plot (discrete-histogram (map list (map car all-together) (map caddr all-together)))
        #:title "Income vs. County Code"
        #:x-label "County Code"
        #:y-label "Income"
        #:width 3000))
;a visual representation of each county (represented by the FIPS code)'s income bracket.


;;; Procedure:
;;;   member?
;;; Parameters:
;;;   x, a value
;;;   lst, a list
;;; Purpose:
;;;   To see if x is one of the elments of lst.
;;; Produces:
;;;   inside?, a predicate
;;; Preconditions:
;;;   No additional preconditions
;;; Postconditions:
;;;   If there are multiple copies of x in lst, it will return #t.
;;;   If #f, there is no x within lst.
(define member?
  (lambda (x lst)
    (if (member x lst)
        #t
        #f)))

;;; Procedure:
;;;  sorted-by-county-codes-within-bracket
;;; Parameters:
;;;   counties-in-a-bracket, a list of FIPS codes that represents counties.
;;; Purpose:
;;;   Returns a table of lists from sorted-by-county-codes that has FIPS codes of counties in a given bracket.
;;; Produces:
;;;   smallers-sorted, a table
;;; Preconditions:
;;;   No additional preconditions.
;;; Postconditions:
;;;   The first element in each list in smaller-sorted contains the FIPS code of a county in a given income bracket.
;;;   The second element in each list in smaller-sorted contains a tech-code.
(define sorted-by-county-codes-within-bracket
  (lambda (counties-in-a-bracket)
    (let kernel [(old sorted-by-county-code)
                 (new null)]
      (cond
        [(null? old)
         (reverse new)]
        [(member? (car (car old)) counties-in-a-bracket)
         (kernel (cdr old) (cons (car old) new))]
        [else
         (kernel (cdr old) new)]))))


;;; Procedure:
;;;   amount
;;; Parameters:
;;;   sorted-by-county-code, the output from the procedure sorted-by-county-codes-within-bracket
;;; Purpose:
;;;   Returns a list of the number of each of the fourteen tech-codes.
;;; Produces:
;;;   counts, a list
;;; Preconditions:
;;;   Essentially the postconditions of sorted-by-county-codes-within-bracket
;;; Postconditions:
;;;   The length of counts is 14.
;;;   The first value in counts is the number of tech-codes with 10 in the given income bracket, the second value, is 11, the third is 12, and etc.
#|
The tech codes displaying which internet connections were used

10 = Asymmetric xDSL
11 = ADSL2, ADSL2+
12 = VDSL
20 = Symmetric xDSL*
30 = Other Copper Wireline (all copper-wire based technologies
other than xDSL; Ethernet over copper and T-1 are examples)
40 = Cable Modem other than DOCSIS 1, 1.1, 2.0, 3.0 or 3.1
41 = Cable Modem – DOCSIS 1, 1.1 or 2.0
42 = Cable Modem – DOCSIS 3.0
43 = Cable Modem – DOCSIS 3.1
50 = Optical Carrier / Fiber to the end user (Fiber to the home or
business end user, does not include “fiber to the curb”)
60 = Satellite†
70 = Terrestrial Fixed Wireless
90 = Electric Power Line
0 = All Other
|#
(define amount
  (lambda (sorted-by-county-code) 
    (map (o (section - <> 1) cadr) (tally-all (map cadr (append placeholders sorted-by-county-code))))))

(define codes ;a list of the 14 tech codes
  (map car (tally-all (map cadr (append placeholders sorted-by-county-code)))))

;;; Procedure:
;;;   total-tally-codes
;;; Parameters:
;;;   counties-in-a-bracket, a list of FIPS codes that represents counties.
;;; Purpose:
;;;   Returns a table with lists of length 2.
;;;   The first element in the list is the tech-code.
;;;   The second element in the list is the amount.
;;; Produces:
;;;   tallied-codes, a table
;;; Preconditions:
;;;   No additional preconditions.
;;; Postconditions:
;;;   The length of tallied-codes is 14.
;;;   (map car (total-tally-codes counties-in-a-bracket)) is in the same order as the list of tech codes (10, 11, 12, etc)
(define total-tally-codes
  (lambda (counties-in-a-bracket)
    (map list codes (amount (sorted-by-county-codes-within-bracket counties-in-a-bracket)))))

;;; Procedure:
;;;   codes-bracket-plot
;;; Parameters:
;;;   counties-in-a-bracket, a list of FIPS codes that represents counties.
;;; Purpose:
;;;   Returns a plot comparing the number of each of the 14 tech codes in a county.
;;; Produces:
;;;   codes-in-bracket, a plot.
;;; Preconditions:
;;;   No additional preconditions.
;;; Postconditions:
;;;   length of x axis is 14.
(define codes-bracket-plot
  (lambda (counties-in-a-bracket)
    (plot (discrete-histogram (total-tally-codes counties-in-a-bracket))
          #:title "Number of Different Tech Codes in a Certain Income Bracket"
          #:x-label "Tech Codes"
          #:y-label "Amount")))

(define all-brackets ;a stacked hisogram of all the tallied tech codes in all of the income brackets.
  (plot (stacked-histogram (list (list "$30000" (map cadr (total-tally-codes 30000-counties)))
                                 (list "$35000" (map cadr (total-tally-codes 35000-counties)))
                                 (list "$40000" (map cadr (total-tally-codes 40000-counties)))
                                 (list "$45000" (map cadr (total-tally-codes 45000-counties)))
                                 (list "$50000" (map cadr (total-tally-codes 50000-counties)))
                                 (list "$55000" (map cadr (total-tally-codes 55000-counties)))
                                 (list "$60000" (map cadr (total-tally-codes 60000-counties))))
                           #:labels (list
                                     "Others"
                                     "Asymmetric xDSL"
                                     "ADSL2"
                                     "VDSL"
                                     "Other Copper Wireline"
                                     "Cable Modem Others"
                                     "Cable 1, 1.1, 2.0"
                                     "Cable 3.0"
                                     "Cable 3.1"
                                     "Optical Carrier"
                                     "Satellite"
                                     "Terrestrial Fixed Wireless"
                                     "Electrical Power Line")
                                     )
        #:title "Number of Different Tech Codes used in Iowa Counties of Different Income Brackets"
        #:x-label "Counties in Income Brackets"
        #:y-label "Number of Tech Codes"))