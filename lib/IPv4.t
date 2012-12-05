module IPv4 where

import BitOps
import Binary
import Ethernet

data IPv4Address = IPv4Address BITS8 BITS8 BITS8 BITS8

instance Eq IPv4Address where
    IPv4Address a0 a1 a2 a3 == IPv4Address b0 b1 b2 b3  
        =  a0==b0 && a1==b1 && a2==b2 && a3==b3
    addr1 /= addr2
        =  not (addr1 == addr2)

ipv4_address :: BITS32 -> IPv4Address
ipv4_address bits =
    IPv4Address
        (to8 (bits .>>. 24))
        (to8 ((bits .>>. 16) .&. 0xff))
        (to8 ((bits .>>. 8) .&. 0xff))
        (to8 (bits .&. 0xff))

ipv4_address_bits32 :: IPv4Address -> BITS32
ipv4_address_bits32 (IPv4Address b0 b1 b2 b3) =
    (to32 b0 .<<. 24) .|. (to32 b1 .<<. 16) .|. (to32 b2 .<<. 8) .|. to32 b3

instance Show IPv4Address where
    show (IPv4Address b0 b1 b2 b3) =
        show (toInt b0) ++ "." ++
        show (toInt b1) ++ "." ++
        show (toInt b2) ++ "." ++
        show (toInt b3)

struct IPv4Header where
    version     :: BITS32
    ihl         :: Int
    tos         :: BITS32
    length      :: Int
    ident       :: BITS32
    flags       :: BITS32
    offset      :: Int
    ttl         :: Int
    protocol    :: Int
    checksum    :: BITS16
    source      :: IPv4Address
    dest        :: IPv4Address

instance Show IPv4Header where
    show (IPv4Header { .. }) =
        "{ " ++
        "version = "    ++ showhex (to8 version)    ++ ", " ++
        "ihl = "        ++ show ihl                 ++ ", " ++
        "tos = "        ++ showhex (to8 tos)        ++ ", " ++
        "length = "     ++ show length              ++ ", " ++
        "ident = "      ++ showhex (to8 ident)      ++ ", " ++
        "flags = "      ++ showhex (to8 flags)      ++ ", " ++
        "offset = "     ++ show offset              ++ ", " ++
        "ttl = "        ++ show ttl                 ++ ", " ++
        "protocol = "   ++ show protocol            ++ ", " ++
        "checksum = "   ++ showhex (to16 checksum)  ++ ", " ++
        "source = "     ++ show source              ++ ", " ++
        "dest = "       ++ show dest                ++
        " }"

instance BinString IPv4Header where
    pack (IPv4Header { .. }) = binBuf 20 (array [w0, w1, w2, w3, w4])
      where w0 = fromInt ihl .|. (version .<<. 4) .|. (tos .<<. 8) .|. (to32 (hton16 (fromInt length)) .<<. 16)
            w1 = ident .|. (flags .<<. 21) .|. (fromInt offset .<<. 16)
            w2 = fromInt ttl .|. (fromInt protocol .<<. 8) .|. (to32 checksum .<<. 16)
            w3 = hton32 (ipv4_address_bits32 source)
            w4 = hton32 (ipv4_address_bits32 dest)
    
    unpack buf off 
      | invalid         = Nothing
      | otherwise       = Just (ihl*4, header)
      where invalid     = ihl < 5 || buf.nbytes-off < ihl*4 || buf.nbytes-off < length
            header      = IPv4Header { .. }
            w0          = extract32 buf.words off
            w1          = extract32 buf.words (off + 4)
            w2          = extract32 buf.words (off + 8)
            w3          = extract32 buf.words (off + 12)
            w4          = extract32 buf.words (off + 16)
            -- First 32 bit word contains following fields:
            -- Version:4, IHL:4, TOS:8, and Total Length:16
            ihl         = toInt (smask32 w0 0  0xf)
            version     = smask32 w0 4  0xf
            tos         = smask32 w0 8  0xff
            length      = toInt (ntoh16 (to16 (smask32 w0 16 0xffff)))
            -- Second 32 bit word contains following fields:
            -- Identification:16, Flags:3, and Offset:13
            ident       = smask32 w1 0 0xff1f
            flags       = smask32 w1 5 0x70
            offset      = toInt (ntoh16 (to16 (smask32 w1 16 0xff1f)))
            -- Third 32 bit word contains following fields:
            -- TTL:8, Protocol:8, and Checksum: 16
            ttl         = toInt (smask32 w2 0 0xff)
            protocol    = toInt (smask32 w2 8 0xff)
            checksum    = to16 (smask32 w2 16 0xffff)
            -- Fourth 32 bit word contains source address.
            source      = ipv4_address (ntoh32 w3)
            -- Fifth 32 bit word contains destination address.
            dest        = ipv4_address (ntoh32 w4)
            -- The remaining words are options, igonored for now.

checksumIPv4 buf off hlen = binv (cs_add cs0 cs1)
  where cs0 = checksumCore buf.words off 10
        cs1 = checksumCore buf.words (off + 12) (hlen - 12)

struct IPv4PseudoHeader where
    ps_source   :: IPv4Address
    ps_dest     :: IPv4Address
    ps_protocol :: Int
    ps_length   :: Int

pseudo_header ps_source ps_dest ps_protocol ps_length = IPv4PseudoHeader { .. }

pseudofy hdr = { ps_source = hdr.source, 
                 ps_dest = hdr.dest, 
                 ps_protocol = hdr.protocol, 
                 ps_length = (hdr.length - hdr.ihl * 4)
               }

pack_pseudo_header (IPv4PseudoHeader { .. }) = binBuf 12 (array [w0, w1, w2])
  where w0 = hton32 (ipv4_address_bits32 ps_source)
        w1 = hton32 (ipv4_address_bits32 ps_dest)
        ulen = to32 (hton16 (fromInt ps_length))
        w2 = (ulen .<<. 16) .|. (fromInt ps_protocol .<<. 8)

--------------------------------------------------------------------------------

struct IPv4 where
    transmitIPv4 :: IPv4Address -> [BinBuffer] -> Action
    receiveIPv4  :: EthernetHeader -> BinBuffer -> Int -> Action

type ReceiverCallback = IPv4Header -> BinBuffer -> Int -> Action

type EthernetCallback = Int -> [BinBuffer] -> Action

--------------------------------------------------------------------------------

ipv4Layer :: Env -> IPv4Address -> ReceiverCallback -> EthernetCallback -> Class IPv4
ipv4Layer env my_ip receiver ethernet_send = class

    debug = env.debug
    
    valid_header header csum  = header.checksum == csum &&
                                header.version == 0x4 &&
                                header.flags .&. 0x01 == 0 &&
                                header.offset == 0

    receiveIPv4 eth_hdr buf off = action
            
        case eth_hdr.etype of
            0x800 -> case unpack buf off of
                        Just (hlen,hdr) | valid_header hdr (checksumIPv4 buf off hlen) ->
                            send receiver hdr buf (off + hlen)
                        _ -> debug "### Bad IPv4 packet\n"
            0x0806 -> debug "Skipping ARP frame\n"
            _      -> debug "Skipping unknown Ethernet frame\n"


--------------------------------------------------------------------------------

    transmitIPv4 raddr buffers = action
        len = totalLength buffers + 20

        buf0 = pack (IPv4Header { version=4, ihl=5, tos=0, length=len, ident=0, flags=0x2, offset=0, 
                                  ttl=64, protocol=0x11, checksum=0, source=my_ip, dest=raddr })
        csum = binv (checksum buf0)
        
        buf = pack (IPv4Header { version=4, ihl=5, tos=0, length=len, ident=0, flags=0x2, offset=0, 
                                 ttl=64, protocol=0x11, checksum=csum, source=my_ip, dest=raddr })

        send ethernet_send 0x800 (buf:buffers)

        result ()

--------------------------------------------------------------------------------

    result IPv4 {..}

