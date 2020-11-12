;;; Use this to reset.
(setf *set-of-orders* NIL)
(setf *geo-valid-orders* NIL)
(setf *support-orders* NIL)
(setf *move-orders* NIL)
(setf *support-orders* NIL)
(setf *hold-orders* NIL)
(setf *convoy-orders* NIL)
(setf *final-move-orders* NIL)
(setf *final-hold-orders* NIL)

;;; Not enough support,nothing happens
(order-input "France" (list "M" "A" "PAR" "BRE"))

;;; RESET before every set of orders

;;; TEST 1

;;; SET 1 
(order-input "France" (list "M" "A" "PAR" "PIC"))
(order-input "Austria" (list "M" "A" "VIE" "TYR"))

;;; SET 2
(order-input "France" (list "M" "A" "PIC" "BEL"))

;;; SET 3 
(order-input "France" (list "M" "A" "BEL" "RUH"))

;;; SET 4
(order-input "France" (list "M" "A" "RUH"  "MUN")) ;;; FAILS

;;; SET 5
(order-input "France" (list "M" "A" "RUH"  "MUN"))
(order-input "Austria" (list "S" "A" "M" "TYR" "RUH" "MUN"))

;;; TEST 2
;;; SET 1-5 same as TEST 1

;;; SET 6
;;; The first attack will not suceeed.
(order-input "Austria" (list "M" "A" "TYR" "MUN"))
(order-input "Russia" (list "M" "A" "WAR" "SIL"))

;;; SET 7
;;; 2 Units trying to swap, nothing happens
(order-input "France" (list "M" "A" "MUN" "SIL"))
(order-input "Russia" (list "M" "A" "SIL" "MUN"))
(order-input "Russia" (list "M" "A" "MOS" "WAR"))

;;; SET 8
;;; Nothing happens.
(order-input "Germany"  (list "M" "A" "BER" "PRU"))
(order-input "Russia" (list "M" "A" "WAR" "PRU"))

;;;SET 9
;;; Nothing happens.
(order-input "Germany"  (list "M" "A" "BER" "PRU"))
(order-input "Russia" (list "M" "A" "WAR" "PRU"))
(order-input "France" (list "M" "A" "MUN" "BER"))

;;; SET 10
(order-input "Germany"  (list "M" "A" "BER" "PRU"))
(order-input "France" (list "M" "A" "MUN" "BER"))

;;; TEST 3
;;; 1-9 same as 2
;;; SET 10
(order-input "Germany"  (list "M" "A" "BER" "PRU"))
(order-input "Russia" (list "M" "A" "WAR" "PRU")) 
(order-input "Russia" (list "S" "A" "M" "SIL" "WAR" "PRU"))
(order-input "France" (list "M" "A" "MUN" "SIL"))
;;; sup cut, nothing happens

;;; SET 11
(order-input "Germany"  (list "M" "A" "BER" "PRU"))
(order-input "Russia" (list "M" "A" "WAR" "PRU")) 
(order-input "Russia" (list "S" "A" "M" "SIL" "WAR" "PRU"))

;;; Here onwards till SET 23 doesnt work the way it would without
;;; multiple coasts, it can be ignored.
;;; I am leaving it in just as a few sample orders.
;;; The orders will still execute correctly, but probably will
;;; not test what I wanted them to check.
;;; SET 12
(order-input "Russia" (list "M" "A" "PRU" "SWE"))
(order-input "Russia" (list "M" "F" "STP" "BOT"))
;;;SET 13
(order-input "Russia" (list "M" "F" "BOT" "BAL"))
;;;set 14
(order-input "Russia" (list "M" "A" "PRU" "SWE"))
(order-input "Russia" (list "C" "F" "BAL" "PRU" "SWE"))

;;; SET 15
(order-input "Russia" (list "M" "F" "BAL" "BOT"))
(order-input "Russia" (list "M" "A" "SWE" "FIN")
             
;;; SET 16
(order-input "Turkey" (list "M" "F" "ANK" "BLA"))
(order-input "Italy" (list "M" "F" "NAP" "APU"))

 ;;; set 17
 (order-input "Turkey" (list "M" "F" "BLA" "BUL"))
 
 ;;; set 18
 (order-input "Turkey" (list "M" "F" "BUL" "GRE"))
 
 ;;; set 19
 (order-input "Turkey" (list "M" "F" "GRE" "ION"))
 
 ;;; set 20        
(order-input "Italy" (list "M" "F" "APU" "ADR"))
(order-input "Turkey" (list "M" "F" "ION" "ADR"))
 ;;; set 21
(order-input "Turkey" (list "M" "F" "ION" "ADR"))
 
 ;;; set 22
 (order-input "Turkey" (list "M" "F" "ADR" "TRI"))
 (order-input "Italy" (list "M" "A" "VEN" "TRI"))
             
 ;;; set 23
 ;;; equally supported moves, none suceeds
 (order-input "Austria" (list "S" "A" "M" "TYR" "ADR" "TRI"))
 (order-input "Turkey" (list "M" "F" "ADR" "TRI"))
 (order-input "Italy" (list "M" "A" "VEN" "TRI"))
 (order-input "Austria" (list "S" "A" "M" "BUD" "VEN" "TRI"))
 
 
 ;;; RESTART THE GAME
 ;;; SET 1
(order-input "Austria" (list "M" "A" "BUD" "SER"))
(order-input "Turkey" (list "M" "F" "ANK" "BLA"))
(order-input "Russia" (list "M" "A" "MOS" "UKR"))
(order-input "Turkey" (list "M" "F" "CON" "BUL"))
;;;SET2 
;;; nothing
(order-input "Russia" (list "M" "A" "UKR" "RUM"))
(order-input "Turkey" (list "M" "A" "BUL" "RUM"))
;;; SET3
;;; cut support, and multiple moves to same loc. none work.
(order-input "Turkey" (list "S" "A" "M" "BUL" "BLA" "RUM"))
(order-input "Turkey" (list "M" "F" "BLA" "RUM"))
(order-input "Austria" (list "M" "A" "SER" "BUL"))
(order-input "Russia" (list "M" "A" "UKR" "RUM"))
;;;set 4
;;; Support cut but still best move.
(order-input "Turkey" (list "S" "A" "M" "BUL" "BLA" "RUM"))
(order-input "Turkey" (list "M" "F" "BLA" "RUM"))
(order-input "Austria" (list "M" "A" "SER" "BUL"))

;;; Restart the game.
;;; Coast stuff   
;;; Test 1             
;;; set 1
(order-input "Turkey" (list "M" "F" "ANK" "BLA"))
(order-input "Austria" (list "M" "A" "BUD" "GAL"))             
;;; set 2
(order-input "Turkey" (list "M" "F" "BLA" "SEV"))
(order-input "Austria" (list "M" "A" "GAL" "UKR"))            

;;; set 3
(order-input "Austria" (list "M" "A" "UKR" "LVN"))
;;; set 4
(order-input "Austria" (list "M" "A" "LVN" "STP NC"))
;;; set 5
(order-input "Austria" (list "M" "A" "LVN" "STP SC"))
;;; set 6
(order-input "Austria" (list "M" "A" "LVN" "STP SC"))             
(order-input "Russia" (list "S" "A" "M" "MOS" "LVN" "STP SC"))
 ;;; set 7            
(order-input "Austria" (list "M" "A" "STP SC" "LVN"))
             
;;; set 8
(order-input "France" (list "M" "A" "PAR" "BUR"))             
(order-input "Austria" (list "M" "A" "VIE" "TYR"))
;;; set 9            
(order-input "Austria" (list  "S" "A" "M" "TYR" "BUR" "MUN"))
(order-input "France" (list "M" "A" "BUR" "MUN"))
(order-input "Germany" (list "S" "A" "H" "BER" "MUN"))
(order-input "Germany" (list "H" "A" "MUN"))
;;; set 10
(order-input "Austria" (list  "S" "A" "M" "TYR" "BUR" "MUN"))
(order-input "France" (list "M" "A" "BUR" "MUN"))
(order-input "Germany" (list "H" "A" "MUN"))            

;;; NEW GAME             
(order-input "England" (list "M" "F" "WES" "SPA SC"))
(order-input "Germany" (list "M" "A" "POR" "SPA NC"))    