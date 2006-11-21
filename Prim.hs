module Prim where

import Ident


-- Tuples ---------------------------------------------------------------

tuple_con n             = '(' : replicate (n-1) ',' ++ ")"

tuple n                 = tuples ! (n-2)

maxTuple                = 5



-- Types ---------------------------------------------------------------

n0 = 1

idents0@
  [ action,  request,  template,  cmd,  o,  message,  ref,  pid,  pmc,  int,  float,  char,  bool,  list, unit ]

  = zipWith primIdent [n0..]

  [ "Action","Request","Template","Cmd","O","Message","Ref","PID","PMC","Int","Float","Char","Bool","[]","()"  ]


-- Basic terms ---------------------------------------------------------

n1 = n0 + length idents0

idents1@
  [ id,      msgEQ,      msgNE,      pidEQ,      pidNE,      fail,      commit,      match,      fatbar ]

  = zipWith primIdent [n1..]

  [ "primId","primMsgEQ","primMsgNE","primPIDEQ","primPIDNE","primFail","primCommit","primMatch","primFatbar" ]



-- Coercions -----------------------------------------------------------

n2 = n1 + length idents1

idents2@
  [ actToCmd,      actToO,      reqToCmd,      reqToO,      templToCmd,      templToO,      cmdToO,      refToPID ]

  = zipWith primIdent [n2..]

  [ "primActToCmd","primActToO","primReqToCmd","primReqToO","primTemplToCmd","primTemplToO","primCmdToO","primRefToPID" ]



-- instance Num Int ----------------------------------------------------

n3 = n2 + length idents2

idents3@
  [ intPlus,      intMinus,      intTimes,      intDiv,      intMod,      intNeg ]

  = zipWith primIdent [n3..]

  [ "primIntPlus","primIntMinus","primIntTimes","primIntDiv","primIntMod","primIntNeg" ]


-- instance Ord Int ----------------------------------------------------

n4 = n3 + length idents4

idents4@
  [ intEQ,      intNE,      intLT,      intLE,      intGE,      intGT ]

  = primIdents [n4..]

  [ "primIntEQ","primIntNE","primIntLT","primIntLE","primIntGE","primIntGT"]


-- instance Num Float ---------------------------------------------------

n5 = n4 + length idents4

idents5@
  [ floatPlus,      floatMinus,      floatTimes,      floatDiv,      floatNeg ]

  = zipWith primIdent [n5..]

  [ "primFloatPlus","primFloatMinus","primFloatTimes","primFloatDiv","primFloatNeg" ]


-- instance Ord Float ---------------------------------------------------

n6 = n5 + length idents5

idents6@
  [ floatEQ,      floatNE,      floatLT,      floatLE,      floatGE,      floatGT ]

  = zipWith primIdent [n6..]

  [ "primFloatEQ","primFloatNE","primFloatLT","primFloatLE","primFloatGE","primFloatGT" ]


-- Char operations ------------------------------------------------------

n7 = n6 + length idents6

idents7@
  [ charToInt, intToChar ]

  = zipWith primIdent [n7..]

  [ "primCharToInt","primIntToChar" ]


-- 
nil                     = mkIdent "[]"
cons                    = mkIdent ":"

true                    = mkIdent "True"
false                   = mkIdent "False"

aft                     = mkIdent "aft"
bef                     = mkIdent "bef"



firstUnique             =                               200
