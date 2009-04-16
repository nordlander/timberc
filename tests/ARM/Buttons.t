module Buttons where

import ARM

struct Buttons where
  init :: Request ()
--  but1 :: Action -> Request
--  but2 :: Action -> Request
  handle :: Int -> Action -> Request ()

addrIO0IntStatF = 0xE0028088 
addrIO2IntStatF = 0xE00280A8
addrIO0IntStatR = 0xE0028084 
addrIO2IntStatR = 0xE00280A4
addrIO0IntClr = 0xE002808C 
addrIO2IntClr = 0xE00280AC

addrIO0IntEnF = 0xE0028094
addrIO2IntEnF = 0xE00280B4

addrEXTINT = 0xE01FC140

buttons env = 
  class
    defaulthandler = action 

    but1handler := defaulthandler 
    but2handler := defaulthandler 
    joy1handler := defaulthandler 
    joy2handler := defaulthandler 
    joy3handler := defaulthandler 
    joy4handler := defaulthandler 

    inthandler = before (millisec 8) action
      env.debug "inthandler\n"
      s0F <- env.portread addrIO0IntStatF
      s2F <- env.portread addrIO2IntStatF
      s0R <- env.portread addrIO0IntStatR
      s2R <- env.portread addrIO2IntStatR
      env.portwrite addrIO0IntClr (s0F .|. s0R)
      env.portwrite addrIO2IntClr (s2F .|. s2R)
      case toInt (s0F .|. s0R .|. s2F .|. s2R) of
        0x80000  -> but1handler
        0x200000 -> but1handler
        0x10     -> joy1handler
        0x20     -> joy2handler
        0x400    -> joy3handler
        0x800    -> joy4handler
        _        -> 
        


    handle 1 meth = request
      but1handler := meth
      result ()
    handle 2 meth = request
      but2handler := meth
      result ()
    handle 3 meth = request
      joy1handler := meth
      result ()
    handle 4 meth = request
      joy2handler := meth
      result ()
    handle 5 meth = request
      joy3handler := meth
      result ()
    handle 6 meth = request
      joy4handler := meth
      result ()

    handle nr meth = request
      env.debug "undef button"
      result ()

    init = request
      env.portset addrIO2IntEnF 0x80000     -- Set bit 19
      env.portset addrIO2IntEnF 0x200000    -- Set bit 21
      env.portset addrIO0IntEnF 0xc30       -- Set bits 4,5,10,11
      env.portwrite addrEXTINT 0x08         -- Clear EINT3
      env.install 17 inthandler
      result ()

    result Buttons {..}