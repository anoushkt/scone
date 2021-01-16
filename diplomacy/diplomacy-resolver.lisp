;;; remove {great -----power}
;;; -*- Mode:Lisp -*-
;;; Authors: Anoushka Tiwari
;;; This file contains a referee for 'diplomacy'. 
;;; Each player inputs a set of orders, and when all players are done,
;;; simply call the 'execute-orders' function.
;;; The functon is called with the current context (eg:(execute-orders general))
;;; The moves must be inputed in the current context to be considered.
;;; 'execute-orders' will make changes to the game state in a new context, 
;;; and return the context. No changes are made in the initial context's game 
;;; state.
;;; In the testing suite, there is a set of lists to reset before a new round
;;; of orders.

;;; Order format:
;;; To input an order, call order-input as (order-input "Country" order specs).
;;; Note: I know you told me to use colon instead of "Country", but this just 
;;; seemed easier to me. I will explain why when we talk.
;;; Please use the standard abbreviations for regions. They are listed
;;; in alphabetical order.

;;; Description of order specs:
;;; It is always a list.
;;; The first charecter defines the type of order. It can be "M","S","H" or "C".
;;; It is not case sensitive.

;;; Hold order samples: 
;;; (list "H" "A" "KIE")  (An army in Kie will hold)
;;; (list "H" "F" "NTH")  (A fleet in NTH will hold)

;;; Move order samples:
;;; (list "M" "A" "Kie" "Ber") (An army moving from Kie to Ber)
;;; (list "M" "F" "NTH" "Den") (A fleet moving from NTH to Den)

;;; Support order samples:
;;; (list  "S" "A" "M" "TYR" "BUR" "MUN") (An army in Tyr will support a move
;;; from BUR to MUN.
;;; (list "S" "A" "H" "BER" "MUN")) 
;;; An army in BER will support a hold in MUN.

;;; Convoy order examples:
;;; (list "C" "F" "BAL" "PRU" "SWE")
;;; A fleet in BAL will convoy an army from PRU to SWE

;;; Order formatting done.
;;; More information in README.

;;; All spaces on the map are of the {diplomacy region} type.
(new-type {diplomacy region} {thing})
(new-complete-split-subtypes {diplomacy region}
                             '({land region}
                               {water region}
                               {coastal region}))

;;; A diplomacy region could allow armies, fleets, or both. This
;;; depends on whether it is a land, water or coastal region.
(new-type {army legal region} {diplomacy region})
(new-type {fleet legal region} {diplomacy region})
(new-is-a {land region} {army legal region})
(new-is-a {water region} {fleet legal region})
(new-is-a {coastal region} {army legal region})
(new-is-a {coastal region} {fleet legal region})

;;; The type of unit that occupies a region.
(new-type {unit} {thing})
(new-indv {army} {unit})
(new-indv {fleet} {unit})

;;; Every valid region on the map is either a land, water
;;; or coastal region.
;;; Please use only these abbreviations.
;;; They have been alphabetically arranged.
(new-indv {ADR} {water region})
(new-indv {AEG} {water region})
(new-indv {ALB} {coastal region})
(new-indv {ANK} {coastal region})
(new-indv {APU} {coastal region})
(new-indv {ARM} {coastal region})
(new-indv {BAL} {water region})
(new-indv {BAR} {water region})
(new-indv {BEL} {coastal region})
(new-indv {BER} {coastal region})
(new-indv {BLA} {water region})
(new-indv {BOH} {land region})
(new-indv {BRE} {coastal region})
(new-indv {BUD} {land region})
(new-indv {BUL} {coastal region})
(new-indv {BUR} {land region})
(new-indv {CON} {coastal region})
(new-indv {CLY} {coastal region})
(new-indv {DEN} {coastal region})
(new-indv {EAS} {water region})
(new-indv {EDI} {coastal region})
(new-indv {ENG} {water region})
(new-indv {FIN} {coastal region})
(new-indv {GAL} {land region})
(new-indv {GAS} {coastal region})
(new-indv {GRE} {coastal region})
(new-indv {GOL} {water region})
(new-indv {BOT} {water region})
(new-indv {HEL} {water region})
(new-indv {HOL} {coastal region})
(new-indv {ION} {water region})
(new-indv {IRI} {water region})
(new-indv {KIE} {coastal region})
(new-indv {LVP} {coastal region})
(new-indv {LVN} {coastal region})
(new-indv {LON} {coastal region})
(new-indv {MAR} {coastal region})
(new-indv {MID} {water region})
(new-indv {MOS} {land region})
(new-indv {MUN} {land region})
(new-indv {NAP} {coastal region})
(new-indv {NAT} {water region})
(new-indv {NAF} {coastal region})
(new-indv {NTH} {water region})
(new-indv {NWY} {coastal region})
(new-indv {NRG} {water region})
(new-indv {PAR} {land region})
(new-indv {PIC} {coastal region})
(new-indv {PIE} {coastal region})
(new-indv {POR} {coastal region})
(new-indv {PRU} {coastal region})
(new-indv {ROM} {coastal region})
(new-indv {RUH} {land region})
(new-indv {RUM} {coastal region})
(new-indv {SER} {land region})
(new-indv {SEV} {coastal region})
(new-indv {SIL} {land region})
(new-indv {SKA} {water region})
(new-indv {SMY} {coastal region})
(new-indv {SPA} {coastal region})
(new-indv {STP} {coastal region})
(new-indv {SWE} {coastal region})
(new-indv {SYR} {land region})
(new-indv {TRI} {coastal region})
(new-indv {TUN} {coastal region})
(new-indv {TUS} {coastal region})
(new-indv {TYR} {land region})
(new-indv {TYN} {water region})
(new-indv {UKR} {land region})
(new-indv {VEN} {coastal region})
(new-indv {VIE} {land region})
(new-indv {WAL} {coastal region})
(new-indv {WAR} {land region})
(new-indv {WES} {water region})
(new-indv {YOR} {coastal region})

;;; Regions with two coasts.
(new-type {subcoast} {coastal region})
(new-indv {STP NC} {subcoast})
(new-indv {STP SC} {subcoast})
(new-indv {SPA NC} {subcoast})
(new-indv {SPA SC} {subcoast})
(new-indv {BUL EC} {subcoast})
(new-indv {BUL SC} {subcoast})

;;; Each diplomacy player is assigned a great power.
;;; I know you said not to use NONE, and I will remove it once we are 
;;; sure everything works. I use NONE so I knoew I have made moves to 
;;; a region before. With NIL, it is difficult to know whether it is 
;;; my first move to a region.
(new-type {great power} {thing})
(new-indv {Austria} {great power})
(new-indv {England} {great power})
(new-indv {France} {great power})
(new-indv {Germany} {great power})
(new-indv {Italy} {great power})
(new-indv {Russia} {great power})
(new-indv {Turkey} {great power})
(new-indv {NONE} {great power})

;;; A {supply center} is a special diplomacy region. Only some
;;; regions are supply centers and can be used for maintaining
;;; armies and fleets.
(new-type {supply center} {diplomacy region})
(new-is-a {ROM} {supply center})
(new-is-a {BUL EC} {supply center})
(new-is-a {BUL SC} {supply center})
(new-is-a {BUL} {supply center})
(new-is-a {SER} {supply center})
(new-is-a {DEN} {supply center})
(new-is-a {SWE} {supply center})
(new-is-a {NWY} {supply center})
(new-is-a {POR} {supply center})
(new-is-a {SPA NC} {supply center})
(new-is-a {SPA SC} {supply center})
(new-is-a {SPA} {supply center})
(new-is-a {POR} {supply center})
(new-is-a {TUN} {supply center})
(new-is-a {HOL} {supply center})
(new-is-a {BEL} {supply center})
(new-is-a {BER} {supply center})
(new-is-a {KIE} {supply center})
(new-is-a {MUN} {supply center})
(new-is-a {ANK} {supply center})
(new-is-a {CON} {supply center})
(new-is-a {SMY} {supply center})
(new-is-a {EDI} {supply center})
(new-is-a {LVP} {supply center})
(new-is-a {LON} {supply center})
(new-is-a {BUD} {supply center})
(new-is-a {TRI} {supply center})
(new-is-a {VIE} {supply center})
(new-is-a {MOS} {supply center})
(new-is-a {STP NC} {supply center})
(new-is-a {STP SC} {supply center})
(new-is-a {STP} {supply center})
(new-is-a {SEV} {supply center})
(new-is-a {WAR} {supply center})
(new-is-a {NAP} {supply center})
(new-is-a {RUM} {supply center})
(new-is-a {VEN} {supply center})
(new-is-a  {BRE} {supply center})
(new-is-a {PAR} {supply center})
(new-is-a  {MAR} {supply center})

;;; A move is geographically possible between two regions only
;;; if they are adjacent.
;;; However, armies may be convoyed between two non-adjacent
;;; land regions by fleets.
(new-relation {adjacent to} :a-inst-of {diplomacy region}
              :b-inst-of {diplomacy region} :symmetric t)
(new-statement {SYR} {adjacent to} {ARM})
(new-statement {SYR} {adjacent to} {SMY})
(new-statement {SYR} {adjacent to} {EAS})
(new-statement {ARM} {adjacent to} {ANK})
(new-statement {ARM} {adjacent to} {SMY})
(new-statement {ARM} {adjacent to} {SEV})
(new-statement {ARM} {adjacent to} {BLA})
(new-statement {SMY} {adjacent to} {CON})
(new-statement {SMY} {adjacent to} {ANK})
(new-statement {SMY} {adjacent to} {ARM})
(new-statement {SMY} {adjacent to} {AEG})
(new-statement {SMY} {adjacent to} {EAS})
(new-statement {ANK} {adjacent to} {CON})
(new-statement {ANK} {adjacent to} {BLA})
(new-statement {CON} {adjacent to} {AEG})
(new-statement {CON} {adjacent to} {BLA})
(new-statement {SEV} {adjacent to} {BLA})
(new-statement {SEV} {adjacent to} {MOS})
(new-statement {SEV} {adjacent to} {RUM})
(new-statement {SEV} {adjacent to} {UKR})
(new-statement {UKR} {adjacent to} {GAL})
(new-statement {UKR} {adjacent to} {MOS})
(new-statement {UKR} {adjacent to} {WAR})
(new-statement {UKR} {adjacent to} {RUM})
(new-statement {WAR} {adjacent to} {GAL})
(new-statement {WAR} {adjacent to} {MOS})
(new-statement {WAR} {adjacent to} {PRU})
(new-statement {WAR} {adjacent to} {SIL})
(new-statement {WAR} {adjacent to} {LVN})
(new-statement {NAF} {adjacent to} {TUN})
(new-statement {NAF} {adjacent to} {WES})
(new-statement {NAF} {adjacent to} {SPA SC})
(new-statement {MOS} {adjacent to} {LVN})
(new-statement {STP SC} {adjacent to} {MOS})
(new-statement {LVN} {adjacent to} {BAL})
(new-statement {LVN} {adjacent to} {PRU})
(new-statement {LVN} {adjacent to} {BOT})
(new-statement {STP SC} {adjacent to} {LVN})
(new-statement {NAP} {adjacent to} {APU})
(new-statement {NAP} {adjacent to} {ROM})
(new-statement {NAP} {adjacent to} {TYN})
(new-statement {NAP} {adjacent to} {ION})
(new-statement {APU} {adjacent to} {ROM})
(new-statement {APU} {adjacent to} {ADR})
(new-statement {APU} {adjacent to} {VEN})
(new-statement {ROM} {adjacent to} {TUS})
(new-statement {ROM} {adjacent to} {TYN})
(new-statement {TUS} {adjacent to} {TYN})
(new-statement {TUS} {adjacent to} {GOL})
(new-statement {TUS} {adjacent to} {VEN})
(new-statement {TUS} {adjacent to} {PIE})
(new-statement {UKR} {adjacent to} {LVN})
(new-statement {UKR} {adjacent to} {SEV})
(new-statement {VEN} {adjacent to} {ADR})
(new-statement {VEN} {adjacent to} {PIE})
(new-statement {VEN} {adjacent to} {TYR})
(new-statement {VEN} {adjacent to} {TRI})
(new-statement {PIE} {adjacent to} {MAR})
(new-statement {PIE} {adjacent to} {GOL})
(new-statement {PIE} {adjacent to} {TYR})
(new-statement {MUN} {adjacent to} {BUR})
(new-statement {MUN} {adjacent to} {BOH})
(new-statement {MUN} {adjacent to} {TYR})
(new-statement {MUN} {adjacent to} {RUH})
(new-statement {MUN} {adjacent to} {KIE})
(new-statement {MUN} {adjacent to} {SIL})
(new-statement {MUN} {adjacent to} {BER})
(new-statement {SIL} {adjacent to} {PRU})
(new-statement {SIL} {adjacent to} {WAR})
(new-statement {SIL} {adjacent to} {BER})
(new-statement {SIL} {adjacent to} {BOH})
(new-statement {PRU} {adjacent to} {BAL})
(new-statement {PRU} {adjacent to} {BER})
(new-statement {PRU} {adjacent to} {LVN})
(new-statement {BER} {adjacent to} {KIE})
(new-statement {BER} {adjacent to} {BAL})
(new-statement {RUH} {adjacent to} {KIE})
(new-statement {RUH} {adjacent to} {BEL})
(new-statement {RUH} {adjacent to} {HOL})
(new-statement {RUH} {adjacent to} {BUR})
(new-statement {KIE} {adjacent to} {SIL})
(new-statement {BLA} {adjacent to} {RUM})
(new-statement {BUL EC} {adjacent to} {BLA})
(new-statement {KIE} {adjacent to} {DEN})
(new-statement {KIE} {adjacent to} {HOL})
(new-statement {KIE} {adjacent to} {BAL})
(new-statement {KIE} {adjacent to} {HEL})
(new-statement {MAR} {adjacent to} {PIE})
(new-statement {MAR} {adjacent to} {BUR})
(new-statement {MAR} {adjacent to} {GAS})
(new-statement {MAR} {adjacent to} {GOL})
(new-statement {MAR} {adjacent to} {SPA NC})
(new-statement {BUR} {adjacent to} {PAR})
(new-statement {BUR} {adjacent to} {GAS})
(new-statement {BUR} {adjacent to} {PIC})
(new-statement {PAR} {adjacent to} {PIC})
(new-statement {PAR} {adjacent to} {GAS})
(new-statement {PAR} {adjacent to} {BRE})
(new-statement {BRE} {adjacent to} {GAS})
(new-statement {BRE} {adjacent to} {PIC})
(new-statement {BRE} {adjacent to} {ENG})
(new-statement {BRE} {adjacent to} {MID})
(new-statement {PIC} {adjacent to} {BEL})
(new-statement {PIC} {adjacent to} {BUR})
(new-statement {PIC} {adjacent to} {ENG})
(new-statement {LON} {adjacent to} {WAL})
(new-statement {LON} {adjacent to} {YOR})
(new-statement {LON} {adjacent to} {LVP})
(new-statement {LON} {adjacent to} {ENG})
(new-statement {LON} {adjacent to} {NTH})
(new-statement {YOR} {adjacent to} {LVP})
(new-statement {YOR} {adjacent to} {NTH})
(new-statement {YOR} {adjacent to} {WAL})
(new-statement {YOR} {adjacent to} {EDI})
(new-statement {YOR} {adjacent to} {CLY})
(new-statement {LVP} {adjacent to} {WAL})
(new-statement {LVP} {adjacent to} {EDI})
(new-statement {LVP} {adjacent to} {IRI})
(new-statement {LVP} {adjacent to} {CLY})
(new-statement {WAL} {adjacent to} {IRI})
(new-statement {WAL} {adjacent to} {ENG})
(new-statement {CLY} {adjacent to} {NAT})
(new-statement {EDI} {adjacent to} {NRG})
(new-statement {EDI} {adjacent to} {CLY})
(new-statement {EDI} {adjacent to} {NTH})
(new-statement {EDI} {adjacent to} {NAT})
(new-statement {BUD} {adjacent to} {VIE})
(new-statement {BUD} {adjacent to} {GAL})
(new-statement {BUD} {adjacent to} {TRI})
(new-statement {BUD} {adjacent to} {SER})
(new-statement {BUD} {adjacent to} {RUM})
(new-statement {GAL} {adjacent to} {BOH})
(new-statement {GAL} {adjacent to} {VIE})
(new-statement {GAL} {adjacent to} {WAR})
(new-statement {GAL} {adjacent to} {UKR})
(new-statement {GAL} {adjacent to} {SIL})
(new-statement {TRI} {adjacent to} {VIE})
(new-statement {TRI} {adjacent to} {TYR})
(new-statement {TRI} {adjacent to} {ADR})
(new-statement {TRI} {adjacent to} {BUD})
(new-statement {VIE} {adjacent to} {BOH})
(new-statement {VIE} {adjacent to} {TYR})
(new-statement {BOH} {adjacent to} {TYR})
(new-statement {GRE} {adjacent to} {SER})
(new-statement {GRE} {adjacent to} {ALB})
(new-statement {GRE} {adjacent to} {ION})
(new-statement {GRE} {adjacent to} {AEG})
(new-statement {BUL SC} {adjacent to} {GRE})
(new-statement {SER} {adjacent to} {BUD})
(new-statement {SER} {adjacent to} {ALB})
(new-statement {SER} {adjacent to} {BUL SC})
(new-statement {SER} {adjacent to} {RUM})
(new-statement {SER} {adjacent to} {BUD})
(new-statement {SER} {adjacent to} {TRI})
(new-statement {ALB} {adjacent to} {ADR})
(new-statement {ALB} {adjacent to} {TRI})
(new-statement {FIN} {adjacent to} {BOT})
(new-statement {FIN} {adjacent to} {SWE})
(new-statement {STP SC} {adjacent to} {FIN})
(new-statement {SWE} {adjacent to} {BOT})
(new-statement {SWE} {adjacent to} {BAL})
(new-statement {SWE} {adjacent to} {NWY})
(new-statement {SWE} {adjacent to} {SKA})
(new-statement {BAL} {adjacent to} {BOT})
(new-statement {GRE} {adjacent to} {ION})
(new-statement {ION} {adjacent to} {NAP})
(new-statement {ION} {adjacent to} {ALB})
(new-statement {DEN} {adjacent to} {BAL})
(new-statement {DEN} {adjacent to} {NTH})
(new-statement {DEN} {adjacent to} {SKA})
(new-statement {HOL} {adjacent to} {KIE})
(new-statement {HOL} {adjacent to} {BEL})
(new-statement {HOL} {adjacent to} {RUH})
(new-statement {HOL} {adjacent to} {NTH})
(new-statement {HOL} {adjacent to} {HEL})
(new-statement {BEL} {adjacent to} {ENG})
(new-statement {BEL} {adjacent to} {RUH})
(new-statement {BEL} {adjacent to} {PIC})
(new-statement {POR} {adjacent to} {MID})
(new-statement {SPA NC} {adjacent to} {POR})
(new-statement {WES} {adjacent to} {NAF})
(new-statement {WES} {adjacent to} {TUN})
(new-statement {WES} {adjacent to} {GOL})
(new-statement {WES} {adjacent to} {TYN})
(new-statement {SPA SC} {adjacent to} {WES})
(new-statement {TYN} {adjacent to} {TUN})
(new-statement {STP NC} {adjacent to} {BAR})
(new-statement {STP NC} {adjacent to} {NWY})
(new-statement {BAR} {adjacent to} {NRG})
(new-statement {BAR} {adjacent to} {SWE})
(new-statement {NWY} {adjacent to} {NRG})
(new-statement {MID} {adjacent to} {NRG})
(new-statement {NTH} {adjacent to} {NRG})
(new-statement {EDI} {adjacent to} {NRG})
(new-statement {STP SC} {adjacent to} {BOT})
(new-statement {STP SC} {adjacent to} {LVN})
(new-statement {STP SC} {adjacent to} {MOS})
(new-statement {STP NC} {adjacent to} {BAR})
(new-statement {STP NC} {adjacent to} {FIN})
(new-statement {SPA NC} {adjacent to} {GAS})
(new-statement {SPA NC} {adjacent to} {MID})
(new-statement {SPA NC} {adjacent to} {MID})
(new-statement {SPA NC} {adjacent to} {GAS})
(new-statement {SPA NC} {adjacent to} {MAR})
(new-statement {SPA SC} {adjacent to} {WES})
(new-statement {SPA SC} {adjacent to} {NAF})
(new-statement {BUL SC} {adjacent to} {AEG})
(new-statement {BUL SC} {adjacent to} {CON})
(new-statement {BUL SC} {adjacent to} {GRE})
(new-statement {BUL EC} {adjacent to} {CON})
(new-statement {BUL EC} {adjacent to} {RUM})
(new-statement {BUL EC} {adjacent to} {BLA})
(new-statement {BUL EC} {adjacent to} {SER})
(new-statement {BUL SC} {adjacent to} {SER})

;;; The {occupying unit} of a {diplomacy region}
;;; tells us whether the region is occupied by an army
;;; or a fleet.
;;; A land region can only be occupied by an army.
;;; A water region can only be occupied by a fleet.
;;; A coastal region can be occupied by an army or a fleet.
(new-indv-role {occupying unit} {diplomacy region} {unit})

;;; This is the great power that owns the unit in the region.
(new-indv-role {occupying great power} {diplomacy region} {great power})

;;; The {current owner} of a {supply center} is the {great power}
;;; that owns it. It is not necessary that an army or fleet of the
;;; currently occupies the region
;;; A center may be occupied by another power without transfer
;;; of ownership during certain phases of the game.
(new-indv-role {current owner} {supply center} {great power})

;;; Initialising supply centers.
(x-is-the-y-of-z {Germany} {current owner} {BER})
(x-is-the-y-of-z {Germany} {occupying great power} {BER})
(x-is-the-y-of-z {army} {occupying unit} {BER})

(x-is-the-y-of-z {Germany} {current owner} {KIE})
(x-is-the-y-of-z {Germany} {occupying great power} {KIE})
(x-is-the-y-of-z {fleet} {occupying unit} {KIE})

(x-is-the-y-of-z {Germany} {current owner} {MUN})
(x-is-the-y-of-z {Germany} {occupying great power} {MUN})
(x-is-the-y-of-z {army} {occupying unit} {MUN})

(x-is-the-y-of-z {Turkey} {current owner} {ANK})
(x-is-the-y-of-z {Turkey} {occupying great power} {ANK})
(x-is-the-y-of-z {fleet} {occupying unit} {ANK})

(x-is-the-y-of-z {Turkey} {current owner} {CON})
(x-is-the-y-of-z {Turkey} {occupying great power} {CON})
(x-is-the-y-of-z {army} {occupying unit} {CON})

(x-is-the-y-of-z {Turkey} {current owner} {SMY})
(x-is-the-y-of-z {Turkey} {occupying great power} {SMY})
(x-is-the-y-of-z {army} {occupying unit} {SMY})

(x-is-the-y-of-z {England} {current owner} {EDI})
(x-is-the-y-of-z {England} {occupying great power} {EDI})
(x-is-the-y-of-z {fleet} {occupying unit} {EDI})

(x-is-the-y-of-z {England} {current owner} {LVP})
(x-is-the-y-of-z {England} {occupying great power} {LVP})
(x-is-the-y-of-z {army} {occupying unit} {LVP})

(x-is-the-y-of-z {England} {current owner} {LON})
(x-is-the-y-of-z {England} {occupying great power} {LON})
(x-is-the-y-of-z {fleet} {occupying unit} {LON})

(x-is-the-y-of-z {Austria} {current owner} {BUD})
(x-is-the-y-of-z {Austria} {occupying great power} {BUD})
(x-is-the-y-of-z {army} {occupying unit} {BUD})

(x-is-the-y-of-z {Austria} {current owner} {TRI})
(x-is-the-y-of-z {Austria} {occupying great power} {TRI})
(x-is-the-y-of-z {fleet} {occupying unit} {TRI})

(x-is-the-y-of-z {Austria} {current owner} {VIE})
(x-is-the-y-of-z {Austria} {occupying great power} {VIE})
(x-is-the-y-of-z {army} {occupying unit} {VIE})

(x-is-the-y-of-z {Russia} {current owner} {MOS})
(x-is-the-y-of-z {Russia} {occupying great power} {MOS})
(x-is-the-y-of-z {army} {occupying unit} {MOS})

(x-is-the-y-of-z {Russia} {current owner} {STP})
(x-is-the-y-of-z {Russia} {occupying great power} {STP})
(x-is-the-y-of-z {fleet} {occupying unit} {STP})


(x-is-the-y-of-z {Russia} {current owner} {STP NC})
(x-is-the-y-of-z {Russia} {occupying great power} {STP NC})
(x-is-the-y-of-z {fleet} {occupying unit} {STP NC})

(x-is-the-y-of-z {Russia} {current owner} {SEV})
(x-is-the-y-of-z {Russia} {occupying great power} {SEV})
(x-is-the-y-of-z {fleet} {occupying unit} {SEV})

(x-is-the-y-of-z {Russia} {current owner} {WAR})
(x-is-the-y-of-z {Russia} {occupying great power} {WAR})
(x-is-the-y-of-z {army} {occupying unit} {WAR})

(x-is-the-y-of-z {Italy} {current owner} {NAP})
(x-is-the-y-of-z {Italy} {occupying great power} {NAP})
(x-is-the-y-of-z {fleet} {occupying unit} {NAP})

(x-is-the-y-of-z {Italy} {current owner} {ROM})
(x-is-the-y-of-z {Italy} {occupying great power} {ROM})
(x-is-the-y-of-z {army} {occupying unit} {ROM})

(x-is-the-y-of-z {Italy} {current owner} {VEN})
(x-is-the-y-of-z {Italy} {occupying great power} {VEN})
(x-is-the-y-of-z {army} {occupying unit} {VEN})

(x-is-the-y-of-z {France} {current owner} {BRE})
(x-is-the-y-of-z {France} {occupying great power} {BRE})
(x-is-the-y-of-z {fleet} {occupying unit} {BRE})

(x-is-the-y-of-z {France} {current owner} {MAR})
(x-is-the-y-of-z {France} {occupying great power} {MAR})
(x-is-the-y-of-z {army} {occupying unit} {MAR})

(x-is-the-y-of-z {France} {current owner} {PAR})
(x-is-the-y-of-z {France} {occupying great power} {PAR})
(x-is-the-y-of-z {army} {occupying unit} {PAR})

;;; Some global variables for order execution.
(defparameter *set-of-orders* NIL)
(defparameter *geo-valid-orders* NIL)
(defparameter *support-orders* NIL)
(defparameter *convoy-orders* NIL)
(defparameter *move-orders* NIL)
(defparameter *hold-orders* NIL)
(defparameter *final-move-orders* NIL)
(defparameter *final-hold-orders* NIL)

(defun equal-diplomacy-elements (elem1 elem2)
  "This function returns true if two diplomacy elements are equal
   and NIL otherwise."
  (eq (lookup-element elem1) (lookup-element elem2)))

(defun not-dip (x)
  (if (equal x :maybe)
  (return-from not-dip t))
  (if (equal x :yes)
  (return-from not-dip NIL))
  (if (equal x :no)
  (return-from not-dip t)))

;;; The initial location of an order is defined as the location at
;;;which the ordered unit currently exists.
(defun get-initial-location (order-line)
  "This function returns the initial location of an order."
  ;;; Hold order.
  (when (equal (first order-line) "H")
    (return-from get-initial-location (third order-line)))
  ;;; Move order.
  (when (equal (first order-line) "M")
      (return-from get-initial-location (third order-line)))
  ;;; Support order.
  (when (equal (first order-line) "S")
      (return-from get-initial-location (fourth order-line)))
  ;;; Convoy order.
  (when (equal (first order-line) "C")
      (return-from get-initial-location (third order-line))))

(defun type-of-support (order-line)
  "This function returns whether the support order is for a move(M)
   or hold(H)."
  (third order-line))

;;; The final location of an order is described as follows:
;;;       MOVE order: The proposed final destination of a unit.
;;;       SUPPORT order: The proposed final destination of the order that
;;;                      is being supported. The supporting unit does not
;;;                      actually change its location to the final location.
;;;       HOLD order: The final location is the same as the initial location.
;;;       CONVOY order: The final location is the final destination of the army
;;;                     that is being convoyed. The convoying fleet does not
;;;                     actually change its location.
(defun get-final-location (order-line)
  "This function returns the final location (as defined above) of an order."
  ;;; Hold order.
  (when (equal (first order-line) "H")
    (return-from get-final-location (third order-line)))
  ;;; Move order.
  (when (equal (first order-line) "M")
    (return-from get-final-location (fourth order-line)))
  ;;; Support order.
  (when (equal (first order-line) "S")
    (if (equal (type-of-support order-line) "H") (return-from get-final-location (fifth order-line)))
    (return-from get-final-location (sixth order-line))

    )
  ;;; Convoy order.
  (when (equal (first order-line) "C")
    (return-from get-final-location (fifth order-line))))


;;; Some helper functions.
(defun convoyee-initial-location(order-line)
  "This function returns the initial location of the army that is being
   convoyed."
  (fourth order-line))
(defun supportee-initial-location (order-line)
  "This function returns the initial location of the unit that is being
   supported."
  (fifth order-line))
(defun is-hold(order-line)
  "This function returns true if the order is a hold."
  (equal (first order-line) "H"))
(defun is-support(order-line)
  "This function returns true if the order is a support."
  (equal (first order-line) "S"))
(defun is-move(order-line)
  "This function returns true if the order is a move."
  (equal (first order-line) "M"))
(defun is-convoy(order-line)
  "This function returns true if the order is a convoy."
  (equal (first order-line) "C"))
(defun coast-valid (move-to-check)
  "This function returns true if a convoying fleet is adjacent to the army
   that is being convoyed."
  (statement-true? (convoyee-initial-location move-to-check)
                 {adjacent to} (get-initial-location move-to-check)))

(defun get-whole-unit(x)
  "This function returns the main region a subcoast belongs to.
    It is basically just the input with the coast spec removed."
  (if (not-dip (is-x-a-y? x {subcoast})) (return-from get-whole-unit x))
  (if (or(equal-diplomacy-elements x {STP NC})
         (equal-diplomacy-elements x {STP SC}))
    (return-from get-whole-unit "STP"))
  (if (or(equal-diplomacy-elements x {BUL EC})
         (equal-diplomacy-elements x {BUL SC}))
    (return-from get-whole-unit "BUL"))
  (if (or (equal-diplomacy-elements x {SPA NC})
          (equal-diplomacy-elements x {SPA SC}))
    (return-from get-whole-unit "SPA")))

(defun geo-ok? (move-to-check)
  "This function returns true if an order is geographically
   valid without a convoy"
  ;;; Hold orders are geographically valid.
  (if (equal (get-initial-location move-to-check)
             (get-final-location move-to-check)) (return-from geo-ok? t))
  ;;; For convoy orders, check an additional condition.
  (if(and (is-convoy move-to-check)
           (not (coast-valid move-to-check))) (return-from geo-ok? NIL))
  (statement-true? (get-initial-location move-to-check)
                   {adjacent to} (get-final-location move-to-check)))

(defun valid-support (x)
  "This function takes in a {support} order and checks if the support is valid
   or being cut in any way. A support is cut if there is a geographically valid
   attack on the supporting unit, even if the attack is not successful."
  (if (not (is-support x)) (return-from valid-support NIL))
  (loop for y in *final-move-orders*
    do (if (equal (get-whole-unit (get-final-location y)) (get-whole-unit (get-initial-location x)))
      (return-from valid-support NIL)))
  (return-from valid-support t))

(defun orders-to (location)
  "This function returns a list containing all the geographically valid
   orders to a location."
  (let ((orders-to-loc NIL))
    (loop for x in *set-of-orders*
      do (if (and (geo-ok? x)
               (is-move x)
               (equal (get-final-location x) location))
        (setf orders-to-loc (append orders-to-loc (list x)))))
    (return-from orders-to orders-to-loc)))

(defun add-support-move (move-order)
  "This function adds all the valid supporters for a move order and places the
   order in a new list."
  (let ((n 0))
    (loop for y in *support-orders*
      do (if (and  (equal (get-final-location move-order) (get-final-location y))
                   (equal (get-initial-location move-order)
                          (supportee-initial-location y))
                           (valid-support y))
                   (setf n (+ n 1))))
    (setf *final-move-orders* 
          (append *final-move-orders* (list (list (first move-order)
                                                  (second move-order)
                                                  (third move-order)
                                                  (fourth move-order) n
                                                  (sixth move-order)))))))

(defun get-move-supporters (move-order)
  "This function adds all the valid supporters for a
   move order and returns the value."
    (let ((n 0))
    (loop for y in *support-orders*
      do (if (and (equal (get-final-location move-order) (get-final-location y))
                  (equal (get-initial-location move-order)
                          (supportee-initial-location y)))
                   (setf n (+ n 1))))
    (return-from get-move-supporters n)))


(defun add-support-hold (hold-order)
"This function adds all the valid supporters for a support order and places the
 order in a new list."
  (let ((n 0))
    (loop for y in *support-orders*
      do (if (and (valid-support y)
                  (equal (get-final-location hold-order) (get-final-location y))
                  (equal (get-initial-location hold-order)
                         (supportee-initial-location y)))
           (setf n (+ n 1))))

    (setf *final-hold-orders* (append *final-hold-orders* 
                                      (list (list (first hold-order)
                                                  (second hold-order)
                                                  (third hold-order)
                                                   n
                                                  (fifth hold-order)))))))

(defun add-support-convoy(convoy-order)
  "This function adds all the valid supporters for
   a convoy order and returns the number."
  (let ((n 0))
    (loop for y in *support-orders*
      do (if (and (valid-support y)
                  (equal (third convoy-order) (get-final-location y))
                  (equal (third convoy-order)(supportee-initial-location y)))
                  (setf n (+ n 1))))
    (return-from add-support-convoy n)))


(defun count-convoy(convoy-order)
  "This function returns true if a
   geographically valid convoy can be succesfully carried out."
  (let ((attacks (orders-to (get-initial-location convoy-order)))
        (no-of-fleet-supporters (add-support-convoy convoy-order)))
    (loop for x in attacks
      do (if (> (get-move-supporters x) no-of-fleet-supporters)
           (return-from count-convoy NIL))))
  (return-from count-convoy t))



(defun valid-convoy-exists(move-to-check)
  "This function returns true if there exists a valid
   convoy for the given move order."
  (loop for x in *convoy-orders*
   do(if (and (equal (convoyee-initial-location x)
                     (get-initial-location move-to-check))
            (equal (get-final-location x) (get-final-location move-to-check))
            (count-convoy x))
       (return-from valid-convoy-exists t))))

(defun geo-ok-with-convoy? (move-to-check)
  "This function returns true if an order is geographically ok with a convoy."
  (if (equal (get-initial-location move-to-check)
             (get-final-location move-to-check))
    (return-from geo-ok-with-convoy? t))
  (if(and (is-convoy move-to-check)
          (not (coast-valid move-to-check)))
    (return-from geo-ok-with-convoy? NIL))
  (or (statement-true? (get-initial-location move-to-check){adjacent to}
                       (get-final-location move-to-check))
      (valid-convoy-exists move-to-check)))

(defun get-other-coast (x)
  "This function returns the other coast for regions with two coasts."
  (if (equal-diplomacy-elements x "STP NC")
    (return-from get-other-coast "STP SC")
    )
  (if (equal-diplomacy-elements x "STP SC")
     (return-from get-other-coast "STP NC"))

  (if (equal-diplomacy-elements x "SPA NC")
      (return-from get-other-coast "SPA SC")
         )
  (if (equal-diplomacy-elements x "SPA SC")
      (return-from get-other-coast "SPA NC"))
  (if (equal-diplomacy-elements x "BUL SC")
      (return-from get-other-coast "BUL EC"))

  (if (equal-diplomacy-elements x "BUL EC")
       (return-from get-other-coast "BUL SC")))


(defun set-other-coast-empty (x)
  "This function sets the owner of the opposite coast to NONE."
  (if (not-dip (is-x-a-y? x {subcoast})) (return-from set-other-coast-empty t))
  (x-is-the-y-of-z "NONE" {occupying great power}  (get-other-coast x)))

(defun make-move (order)
  "This function completes a valid order."
  (let ((initial-location (third order))
        (final-location (fourth order))
        (type-of-unit (second order))
        (owner-of-unit (sixth order)))
      (if (equal type-of-unit "F") (x-is-the-y-of-z
                                    {fleet} {occupying unit} final-location))
      (if (equal type-of-unit "A") (x-is-the-y-of-z {army}
                                                    {occupying unit}
                                                    final-location))
      (x-is-the-y-of-z owner-of-unit {occupying great power} final-location)
      (x-is-the-y-of-z {NONE} {occupying great power} initial-location)
      (x-is-the-y-of-z owner-of-unit {occupying great power} (get-whole-unit final-location))
      (if (equal type-of-unit "F") (x-is-the-y-of-z
                                    {fleet} {occupying unit} (get-whole-unit final-location)))
      (if (equal type-of-unit "A") (x-is-the-y-of-z {army}
                                                    {occupying unit}
                                                    (get-whole-unit final-location)))
      (set-other-coast-empty final-location)
      (set-other-coast-empty initial-location)

      (x-is-the-y-of-z {NONE} {occupying great power} (get-whole-unit initial-location))))

(defun best-move (x)
  "This function returns true if a move order to a location is the strongest
   move to that location"
    (loop for y in *final-move-orders*
         do (if  (equal (get-whole-unit (get-final-location x)) (get-whole-unit(get-final-location y)))
             (if (<= (fifth x) (fifth y))
               (if (not (equal (get-whole-unit (get-initial-location x)) (get-whole-unit(get-initial-location y))))
               (return-from best-move NIL)))))
     (return-from best-move t))

(defun nc-sc-empty (x)
  "This function returns true if a subcoast is unoccupied."
  (if (not-dip (is-x-a-y? x {subcoast})) (return-from nc-sc-empty t))

  (let ((y (get-whole-unit x)))

    (if (not (or (equal (the-x-of-y {occupying great power}
                                                 y) NIL)
                 (equal-diplomacy-elements (the-x-of-y {occupying great power}
                                                       y) "NONE")))
      (return-from nc-sc-empty NIL)))
  (return-from nc-sc-empty t))

(defun final-resolution()
  "This function makes moves once the number of supporters for each
   order has been determined."
   (let  ((p 0))
      (loop for x in *final-move-orders*
        do(when (best-move x)
            (setf p 0)
            (if (= (fifth x) 0)(setf p 1))
            (write "hereee")
            ;;; final unoccupied
            (if (equal-diplomacy-elements (the-x-of-y {occupying great power}
                                                      (get-whole-unit(get-final-location x)))
                                          NIL) (setf p 0))
            (if (equal-diplomacy-elements (the-x-of-y {occupying great power}
                                                        (get-whole-unit(get-final-location x)))
                                                      {NONE}) (setf p 0))
            (loop for y in *final-hold-orders*
              do (
                  if (and (equal (get-final-location x) (get-initial-location y))
                          (<= (fifth x) (fourth y )))
                   (setf p 1))
              )
             ;;; If the move has more supporters than the hold, or
             ;;; there is no hold, make the move.
              (if (eq p 0) (make-move x))))))

(defun get-type-of-unit-ordered (order-line)
  "This function returns the type of unit an order is for."
      (let ((ordered-unit (second order-line )))
        (if(equal ordered-unit "F")
          (return-from get-type-of-unit-ordered {fleet}))
        (return-from get-type-of-unit-ordered {army})))

(defun unit-valid(order-line)
  "This function returns true if the ordered unit can move to the desired
   position."
  (or
   (and (equal-diplomacy-elements (get-type-of-unit-ordered order-line)
                                  {fleet})
        (equal (is-x-a-y? (get-final-location order-line) {fleet legal region})
               :yes))
   (and (equal-diplomacy-elements (get-type-of-unit-ordered order-line) {army})
        (equal (is-x-a-y? (get-final-location order-line) {army legal region})
               :yes))))


;;; Order description at the begining of the file.
(defun order-input (player order-line)
  "This function takes in a diplomacy order and adds it to
   the set of orders to be resolved."
  (setf player (string-capitalize player))
  (setf (first order-line) (string-upcase (first order-line)))
  (setf (second order-line) (string-upcase (second order-line)))
  (setf (third order-line) (string-upcase (third order-line)))


  (when (equal (first order-line) "H")
    (setf order-line (append order-line (list 0))))
  (when (equal (first order-line) "M")
      (setf order-line (append order-line (list 0))))
  (when (equal (first order-line)  "C")
    (setf order-line (append order-line (list 0))))
  (setf order-line (append order-line (list (string-capitalize player))))
  (if (and (equal-diplomacy-elements (get-type-of-unit-ordered order-line)
                                     (the-x-of-y {occupying unit}
                                                 (get-initial-location
                                                  order-line)))
           (equal-diplomacy-elements player (the-x-of-y {occupying great power}
                                                        (get-initial-location
                                                         order-line)))
           (unit-valid order-line))
       (setf *set-of-orders* (append *set-of-orders* (list order-line)))))

(defun execute-orders(current-context)
  "This function takes in a context. The context represents the current game 
   state. It resolves all orders in the set-of-orders list. The resolution
   takes place in a newly created context, and the new context is returned."
  ;;; Store all the geographically valid orders in a list.
  (loop for x in *set-of-orders*
    do (if (geo-ok? x) (setf *geo-valid-orders*
                                    (append *geo-valid-orders* (list x)))))
  ;;; Make lists of {move}, {hold} and {support} orders.
  (loop for x in *geo-valid-orders*
    do (if(is-support x)
         (setf *support-orders* (append *support-orders* (list x)))))

  (loop for x in *geo-valid-orders*
    do (if (is-move x) (setf *move-orders* (append *move-orders* (list x)))))
  (loop for x in *geo-valid-orders*
    do (if (is-hold x) (setf *hold-orders* (append *hold-orders* (list x)))))

  (loop for x in *geo-valid-orders*
    do (if (and (is-convoy x) (count-convoy x)) (setf *convoy-orders*
                                                      (append *convoy-orders*
                                                              (list x)))))
  (setf *move-orders* NIL)
  (setf *final-move-orders* NIL)
  ;;; Some additional move orders may be possible due to convoy orders.
  (loop for x in *set-of-orders*
     do(if (and (is-move x) (geo-ok-with-convoy? x))
         (setf *final-move-orders* (append *final-move-orders* (list x)))))

   ;;; Count the number of supporters for {hold} and {move} orders.
  (loop for x in *final-move-orders* do (add-support-move x))
  (loop for x in *hold-orders* do(add-support-hold x))

  ;;; Change the state of the game based on the supporters and opposers
  ;;; for each order.
  (let ((next-context (new-context NIL current-context)))
    (in-context next-context)
    (final-resolution)
  (return-from execute-orders next-context)))



;;; At this point, we start coding the agent.


(defun get-all-regions()
   (let ((list-of-orders (list)))
      (loop for x in )
   
   )
)

(defun all-move-generator (great-power)
   (let (( locations-power-is-in (list-all-x-inverse-of-y 
        {occupying great power} great-power))
        (possible-moves (list))
        (cur-loc-unit nil))
        (loop for x in locations-power-is-in
           do (setf cur-loc-unit (the-x-of-y {occupying unit} x))
             (setf possible-moves (append possible-moves (list (list "H" cur-loc-unit x ))))
             (let ((possi-regions (list-rel {adjacent to} x)))
                 (loop for y in possi-regions
                    do (setf possible-moves (append possible-moves
                                       (list (list "M" cur-loc-unit x y)))))))
           (return-from all-move-generator possible-moves)))

(defun distance-from (context great-power)
  (let (( locations-power-is-in (list-all-x-inverse-of-y 
        {occupying great power} great-power))
        (total 0))
        (loop for x in locations-power-is-in
          do (if (equal-diplomacy-elements x {PAR}) (setf total (+ 5 total)))

          
        )
        (return-from distance-from total)
  )

)
(defun utility (context great-power)
   (let ((util 0)
         (owned-by-great-power (length (list-all-x-inverse-of-y 
         {occupying great power} great-power))))
    (return-from utility owned-by-great-power)))




(defun predicted-utility (lookahead great-power context move)
   (if (eq lookahead 0)
       (return-from predicted-utility (utility context great-power)))

   (let ((moves (list)) (new-context nil))
        (loop for power in '("ENGLAND" "ITALY" "TURKEY" "GERMANY" "AUSTRIA" "FRANCE")
            do (setf moves (append moves (planner power context (- lookahead 1)))))
        (setf new-context (execute-orders context))
        (return-from predicted-utility (utility new-context great-power))))


(defun planner (great-power context lookahead)
    (let ((all-moves (all-move-generator great-power))
           (suggested-moves (list))
           (current-utility (utility great-power context)))
           (loop for move in all-moves
              do  (if (> (predicted-utility lookahead great-power context move) current-utility)
                  (setf suggested-moves (append suggested-moves (list move)))))
            (return-from planner suggested-moves)))

