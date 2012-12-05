module Ethernet where

import Binary
import LPC24XXEthernetDriver

type EthernetType = Int

struct EthernetHeader where
    dst :: MACAddress
    src :: MACAddress
    etype  :: EthernetType

instance Show EthernetHeader where
    show (EthernetHeader { .. }) =
        "{ " ++
        "dst = "    ++ show dst     ++ ", " ++
        "src = "    ++ show src     ++ ", " ++
        "etype = "  ++ show etype   ++
        " }"

instance BinString EthernetHeader where
    pack (EthernetHeader { .. }) = binBuf 14 (array [w0, w1, w2, w3])
      where MACAddress s0 s1 s2 s3 s4 s5 = src
            MACAddress d0 d1 d2 d3 d4 d5 = dst
            w0 = to32 s0 .|. (to32 s1 .<<. 8) .|. (to32 s2 .<<. 16) .|. (to32 s3 .<<. 24)
            w1 = to32 s4 .|. (to32 s5 .<<. 8) .|. (to32 d0 .<<. 16) .|. (to32 d1 .<<. 24)
            w2 = to32 d2 .|. (to32 d3 .<<. 8) .|. (to32 d4 .<<. 16) .|. (to32 d5 .<<. 24)
            w3 = to32 (hton16 (fromInt etype))
    unpack buf off
      | buf.nbytes - off < 0x3c = Nothing
      | otherwise               = Just (14, EthernetHeader { .. })
      where d0 = extract8 buf.words off
            d1 = extract8 buf.words (off+1)
            d2 = extract8 buf.words (off+2)
            d3 = extract8 buf.words (off+3)
            d4 = extract8 buf.words (off+4)
            d5 = extract8 buf.words (off+5)
            dst = MACAddress d0 d1 d2 d3 d4 d5

            s0 = extract8 buf.words (off+6)
            s1 = extract8 buf.words (off+7)
            s2 = extract8 buf.words (off+8)
            s3 = extract8 buf.words (off+9)
            s4 = extract8 buf.words (off+10)
            s5 = extract8 buf.words (off+11)
            src = MACAddress s0 s1 s2 s3 s4 s5

            etype = toInt (ntoh16 (extract16 buf.words (off+12)))
    
--------------------------------------------------------------------------------

struct Ethernet where
    initializeEth :: Action -> Action
    transmitEth   :: MACAddress -> EthernetType -> [BinBuffer] -> Action
    broadcastEth  :: EthernetType -> [BinBuffer] -> Action

type ReceiverCallback = EthernetHeader -> BinBuffer -> Int -> Action

ethernetLayer :: Env -> Addr -> MACAddress -> ReceiverCallback -> Class Ethernet
ethernetLayer env port_addr my_mac receiver = class

    debug = env.debug
    
    driver = new ethernetDriver env port_addr receiveEth
    
    initializeEth init_done = action
        driver.init debug init_failure (action
            driver.mac_set my_mac
            send driver.init_link debug init_failure init_done)
        
    init_failure str = action
        debug ("Ethernet initialization failed: " ++ str ++ ".\r\n")
    
    receiveEth buf off = action

        case unpack buf off of
            Just (hlen,hdr) -> 
                send receiver hdr buf (off+hlen)
            Nothing -> 
                debug "### Bad Ethernet frame\n"


    transmitEth dst etype buffers = action
        buf = pack (EthernetHeader { src = my_mac, .. })
        driver.transmit (buf:buffers)

    broadcastEth etype buffers = action
        buf = pack (EthernetHeader { src = my_mac, dst = broadcastMAC, .. })
        driver.transmit (buf:buffers)

    result Ethernet {..}
