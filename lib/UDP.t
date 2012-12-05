module UDP where

    import Binary
    import IPv4

    struct UDPHeader where
        sport  :: Int
        dport  :: Int
        totlen :: Int
        csum   :: BITS16

    udp_header sport dport totlen csum = UDPHeader { .. }
    
    instance Show UDPHeader where
        show (UDPHeader { .. }) =
            "{ " ++
            "sport = "  ++ show sport   ++ ", " ++
            "dport = "  ++ show dport   ++ ", " ++
            "totlen = " ++ show totlen     ++ ", " ++
            "csum = "   ++ showhex csum ++
            " }"

    instance BinString UDPHeader where
        pack (UDPHeader { .. }) = binBuf 8 (array [w0, w1])
          where sp = hton16 (fromInt sport)
                dp = hton16 (fromInt dport)
                w0 = to32 sp .|. (to32 dp .<<. 16)
                ulen = hton16 (fromInt totlen)
                w1 = to32 ulen .|. (to32 csum .<<. 16)

        unpack buf off
          | invalid                     = Nothing
          | otherwise                   = Just (8, UDPHeader { .. })
          where invalid                 = buf.nbytes-off < 8
                w0 = extract32 buf.words off
                w1 = extract32 buf.words (off + 4)
                -- First word contains source and destination port.
                sport = toInt (ntoh16 (to16 (smask32 w0 0 0xffff)))
                dport = toInt (ntoh16 (to16 (smask32 w0 16 0xffff)))
                -- Second word contains length and checksum.
                totlen = toInt (ntoh16 (to16 (smask32 w1 0 0xffff)))
                csum = to16 (smask32 w1 16 0xffff)

    checksumUDP buf off hlen = cs_add cs0 cs1
      where cs0 = checksumCore buf.words off 6
            cs1 = checksumCore buf.words (off + 8) (hlen - 8)


--------------------------------------------------------------------------------

    type UDPHost = (IPv4Address, Int)

    struct UDP where
        transmitUDP :: UDPHost -> [BinBuffer] -> Action
        receiveUDP  :: IPv4Header -> BinBuffer -> Int -> Action

    type ReceiverCallback = UDPHost -> BinBuffer -> Int -> Action
    
    type IPv4Callback     = IPv4Address -> [BinBuffer] -> Action
    
--------------------------------------------------------------------------------

    udpLayer :: Env -> UDPHost -> ReceiverCallback -> IPv4Callback -> Class UDP
    udpLayer env (my_ip, my_port) receiver ipv4_send = class
    
        debug = env.debug
        
        valid_header ipv4_hdr hdr buf off
          | hdr.totlen > buf.nbytes         = False
          | hdr.csum == 0                   = True
        valid_header ipv4_hdr hdr buf off   = hdr.csum == csum
          where csum                        = binv (cs_add csum0 csum12)
                csum0                       = checksum (pack_pseudo_header (pseudofy ipv4_hdr))
                csum12                      = checksumUDP buf off hdr.totlen
                
        receiveUDP ipv4_hdr buf off = action

            case ipv4_hdr.protocol of
                0x11  -> case unpack buf off of
                            Just (hlen, hdr) | valid_header ipv4_hdr hdr buf off ->
                                raddr = ipv4_hdr.source
                                rport = hdr.sport
                                send receiver (raddr, rport) buf (off + hlen)
                            _ ->
                                debug "### Bad UDP packet\n"
                
                0x06  -> debug "Skipping TCP packet\n"
                0x01  -> debug "Skipping ICMPv4 packet\n"
                _     -> debug "Skipping unknown IP packet\n"
        

        transmitUDP (raddr, rport) buffers = action
            len = totalLength buffers + 8

            buf0 = pack_pseudo_header (pseudo_header my_ip raddr 0x11 len)
            buf1 = pack (udp_header my_port rport len 0)
            csum = binv (checksums (buf0:buf1:buffers))
            
            buf = pack (udp_header my_port rport len csum)

            send ipv4_send raddr (buf:buffers)

            result ()

        result UDP {..}

