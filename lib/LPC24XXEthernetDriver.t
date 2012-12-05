module LPC24XXEthernetDriver where

    import LPC24XXEthernetPort

    data MACAddress = MACAddress BITS8 BITS8 BITS8 BITS8 BITS8 BITS8

    instance Eq MACAddress where
        MACAddress a0 a1 a2 a3 a4 a5 == MACAddress b0 b1 b2 b3 b4 b5
            = a0==b0 && a1==b1 && a2==b2 && a3==b3 && a4==b4 && a5==b5
        addr1 /= addr2
            = not (addr1 == addr2)

    instance Show MACAddress where
        show (MACAddress b0 b1 b2 b3 b4 b5) =
            "(MACAddress " ++
            showhex b0 ++ ":" ++
            showhex b1 ++ ":" ++
            showhex b2 ++ ":" ++
            showhex b3 ++ ":" ++
            showhex b4 ++ ":" ++
            showhex b5 ++
            ")"

    broadcastMAC = MACAddress 0xff 0xff 0xff 0xff 0xff 0xff
    
--------------------------------------------------------------------------------

    struct Driver where
        init     :: (String -> Request ()) -> (String -> Action) -> Action -> Action
        init_link :: (String -> Request ()) -> (String -> Action) -> Action -> Action
        shutdown :: (String -> Action) -> Action -> Action
        mac_get   :: Request MACAddress
        mac_set   :: MACAddress -> Request ()
        transmit :: [BinBuffer] -> Request ()

--------------------------------------------------------------------------------

    ethernetDriver :: Env -> Addr -> (BinBuffer->Int->Action) -> Class Driver
    ethernetDriver env base_addr receiver = class

--------------------------------------------------------------------------------
        debug = env.debug

        eth = new ethernetPort env base_addr
        

        m_state := Uninitialized

        m_phy_interval = millisec 100
        m_phy_timeout  = sec 3

--------------------------------------------------------------------------------

        m_read_phy :: (String -> Request ()) -> BITS32 -> (String -> Action) -> (BITS32 -> Action) -> Class Action
        m_read_phy debug addr fail_cb done_cb = class

--------------------------------------------------------------------------------

            result action

                -- Write address
                address = addr .|. eth.phy_addr
                eth.madr.write address

                -- Enable continous reads
                eth.mcmd.write 1

                -- Cleanup function, we always disable continues reads.
                cleanup :: (BITS32 -> Action) -> Action
                cleanup act = action
                    -- Disable continous reads
                    eth.mcmd.write 0
                    bits <- eth.mrdd.read
                    send act bits

                -- Poll until read is done (or we timeout).
                poll = new poller
                    debug
                    eth.mind
                    (\x -> x == (fromInt 0))
                    m_phy_interval
                    m_phy_timeout
                    (cleanup (\x -> fail_cb ("Failed to read phy-register " ++ (showhex addr) ++ "\r\n")))
                    (\x -> cleanup done_cb)
                poll

--------------------------------------------------------------------------------

        m_write_phy :: (String -> Request ()) -> BITS32-> BITS32 -> (String -> Action) -> Action -> Class Action
        m_write_phy debug addr bits fail_cb done_cb = class

--------------------------------------------------------------------------------

            result action 

                -- We wish to write, not read.
                eth.mcmd.write 0

                -- Write address and data
                eth.madr.write (eth.phy_addr .|. addr)
                eth.mwtd.write bits

                -- Create a poller that polls until we managed to write
                -- the data, or timeout.
                poll = new poller
                    debug
                    eth.mind
                    (\x -> x == (fromInt 0))
                    m_phy_interval
                    m_phy_timeout
                    (fail_cb ("Failed to write to phy-register " ++ (showhex addr) ++ "\r\n"))
                    (\x -> done_cb)
                poll


--------------------------------------------------------------------------------

        m_phy_reset :: (String -> Request ()) -> (String -> Action) -> Action -> Class Action
        m_phy_reset debug fail_cb done_cb = class

--------------------------------------------------------------------------------

            m_count := 0
            m_count_max = 10

            m_poller = new m_read_phy debug 0 fail_cb m_check

--------------------------------------------------------------------------------

            m_check :: BITS32 -> Action
            m_check bits = action
                m_count := m_count + 1
                if m_count > m_count_max then
                    send fail_cb "Failed to soft reset phy-device.\r\n"
                elsif (bits .&. (fromInt 0x8000)) /= (fromInt 0) then
                    after millisec 100 send m_poller
                else
                    send done_cb

--------------------------------------------------------------------------------

            result action
                reset = new m_write_phy debug 0 0x8000 fail_cb m_poller
                send reset

--------------------------------------------------------------------------------

        m_phy_link :: (String -> Request ()) -> Time -> (String -> Action) -> Action -> Class (Request ())
        m_phy_link debug until fail_cb done_cb = class

--------------------------------------------------------------------------------

            m_failed := False
            m_fail_r := Nothing

            m_int = millisec 20
            m_poller = new m_read_phy debug 0x10 m_fail m_check

--------------------------------------------------------------------------------

            m_fail str = action
                    debug str

--------------------------------------------------------------------------------

            m_check bits = action
                -- We have a race so we need to check the flag first.
                if not m_failed then
                    if (bits .&. 0x11) == 0x11 then
                        --  NOTE: m_fail_r must be valid at all times.
                        abort (fromJust m_fail_r)
                        send done_cb
                    else
                        after m_int send m_poller
                result ()

--------------------------------------------------------------------------------

            result request
                -- Initialize an auto-negotiation of the link,
                -- bit 12 is auto-negotiate and 9 is restart autonegotiation.
                bits = (1 .<<. 12) .|. (1 .<<. 9)
                auto_negotiation = new m_write_phy debug (fromInt 0) bits fail_cb poll_link
                poll_link = action
                    -- Poll the link status until we timeout.
                    after m_int {- before m_int -} send m_poller
                    fail_r = after until send action
                        m_failed := True
                        send fail_cb "Timeout waiting for link."
                    m_fail_r := Just fail_r

                auto_negotiation
                result ()

--------------------------------------------------------------------------------

        init :: (String -> Request ()) -> (String -> Action) -> Action -> Action
        init debug fail_cb done_cb = action
            
            case m_state of
                Uninitialized ->
                    init_do debug fail_cb done_cb
                _ ->
                    fail_cb "Driver not in state Uninitialized."

--------------------------------------------------------------------------------

        init_do debug fail_cb done_cb = do

            -- Mark the state we are in.
            m_state := Init

            -- First power on the eth hardware.
            eth.poweron

            eth_reset_done = action
        
                -- Start out small by setting up the clock for the MII Mgmt.
                -- bits 4:2 are used to set clock, 6 is divided by 20.
                eth.mcfg.write (6 .<<. 2)

                -- Inititate a single read cycle
                eth.mcmd.write 0

                -- Use RMII, instead of MII.
                eth.command.set (1 .<<. 9)

                -- Enable 100 Mbit mode.
                eth.supp.write (1 .<<. 8)

                -- Failure action to set the appropriate state.
                fail str = action
                    m_state := Error str
                    send fail_cb str

                -- What should be done once the reset is successfull.
                phy_reset_done = action
                    -- Read first phy-id and validate.
                    read_phy_id1 = new m_read_phy
                        debug
                        2
                        fail    
                        validate_phy_id1
                    send read_phy_id1
                    result ()

                -- Action to validate phy id1
                validate_phy_id1 id = action
                    if (id .&. (fromInt 0x2000)) /= (fromInt 0x2000) then
                        m_state := Error "Failed to validate phy-id1"
                        send fail_cb ("Failed to validate phy-id1: " ++ (showhex id) ++ "\r\n")
                    else
                        -- Read next phy-id and validate.
                        read_phy_id2 = new m_read_phy
                            debug
                            3
                            fail    
                            validate_phy_id2
                        send read_phy_id2
                    result ()

                -- Action to validate phy id2
                validate_phy_id2 id = action
                    if (id .&. (fromInt 0x5C90)) /= (fromInt 0x5C90) then
                        m_state := Error "Failed to validate phy-id2"
                        send fail_cb ("Failed to validate phy-id2: " ++ (showhex id) ++ "\r\n")
                    else
                        -- Once we get here the mac/eth is ready for more advanced
                        -- setup, such as setting mac address and trying to setup link.
                        m_state := Initialized
                        send read_phycr
                    result ()

                -- Enable the Auto-MDIX configuration.
                read_phycr = new m_read_phy debug 0x19 fail read_phycr_done
                read_phycr_done bits = action
                    write_phycr = new m_write_phy debug 0x19 (bits .|. (1 .<<. 15)) fail done_cb
                    send write_phycr

                -- Reset and poll until reset or timeout.
                phy_reset = new m_phy_reset debug fail phy_reset_done
                send phy_reset

            -- Reset the mac hardware.
            eth.LPC24XXEthernetPort.reset eth_reset_done
            result ()

--------------------------------------------------------------------------------

        init_link debug fail_cb done_cb = action
            
            -- Make sure state is valid.
            case m_state of
                Initialized ->
                    init_link_do debug fail_cb done_cb
                _ ->
                    fail_cb "Driver not in state initialized."
            result ()

--------------------------------------------------------------------------------

        init_link_do debug fail_cb done_cb = do
            phy_link = new m_phy_link debug (sec 10) fail_cb setup_link

            setup_link = action
                read_status = new m_read_phy debug 0x10 fail_cb setup_mac
                send read_status

            setup_mac bits = action
                -- Setup some registers based on the speed and duplex...
                speed = not ((bits .&. 0x2) == 0x2) -- True == 100mbit, False == 10mbit.
                duplex = (bits .&. 0x4) == 0x4 -- False == Half, True Full.

                -- First set options that are only related to the duplex mode.
                if duplex then
                    -- Set full-duplex mode and enable crc and pad.
                    eth.mac2.write 0x31

                    -- Enable passing of runt frames to memory, enable full-duplex and RMII
                    eth.command.set 0x640

                    -- Set the back-to-back inter-packet-gap to reccomended value.
                    eth.ipgt.write 0x15
                else
                    -- Enable crc and pad.
                    eth.mac2.write 0x30

                    -- Enable passing of runt frames to memory, and enable RMII
                    eth.command.set 0x240

                    -- Set the back-to-back inter-packet-gap to reccomended value.
                    eth.ipgt.write 0x12
                
                -- Set options that are based only on the speed.
                if speed then
                    -- Enable 100mbits mode
                    eth.supp.set (1 .<<. 8)
                else
                    -- Disable 100mbits mode.
                    eth.supp.clear (1 .<<. 8)
                
                -- Setup the ethernet memory.
                eth.memory_init

                -- Pass all received frames
                --eth.mac1.set 0x2

                -- Setup the receive filter, we want all broadcast and all that
                -- match our mac-address.
                eth.rxfilterctrl.write 0x22

                -- Clear all interrupts.
                eth.intclear.write 0xffff

                -- Enable all interrupts with the exception of wake-up and
                -- soft-interrupt.
                eth.intenable.write 0xff

                -- Install the interrupt dispatcher.
                eth.interrupt (interrupt_dispatcher eth.intstatus eth.intclear (interrupt_handler debug))

                -- Enable Tx and Tx
                eth.mac1.set 0x1
                eth.command.set 0x3

                -- And we are done, more or less...
                m_state := LinkInitialized
                send done_cb

            phy_link

--------------------------------------------------------------------------------

        interrupt_handler debug status = before (millisec 50) action
            case m_state of
                LinkInitialized ->
                    interrupt_handler_do debug status
                _ ->
                    debug ("Invalid state for interrupt (" ++ (showhex status) ++ ").\r\n")
                    result ()

--------------------------------------------------------------------------------

        interrupt_handler_do debug status = do
            rx_overrun  = 1 .<<. 0 -- Fatal
            rx_done     = 1 .<<. 3
            tx_underrun = 1 .<<. 4 -- Fatal
            tx_done     = 1 .<<. 7

            -- Check for fatal errors.
            if (rx_overrun .|. tx_underrun) .&. status /= 0 then
                -- Fail, hard. Also disable all interrupts.
                eth.intenable.write 0x00
                msg = ("Fatal error (" ++ (showhex status) ++ ").\r\n")
                debug msg
                m_state := Error msg
            else
                -- No fatal errors yet, let us move on.
                if rx_done .&. status /= 0 then
                    -- Schedule reading of memory.
                    read = before sec 1 action
                        read_all = do
                            buf <- eth.memory_read
                            if buf.nbytes > 0 then
                                send receiver buf 0
                                read_all
                        read_all
                    send read

                if tx_done .&. status /= 0 then
                    -- TODO: actually send new data here.

            result ()

--------------------------------------------------------------------------------

        shutdown fail_cb done_cb = action

            -- Make sure state is invalid.
            case m_state of
                Initialized ->
                    shutdown_do fail_cb done_cb
                _ ->
                    fail_cb "Driver not in state initialized."
            result ()
        
--------------------------------------------------------------------------------

        shutdown_do fail_cb done_cb = do
            eth.poweroff
            send done_cb

--------------------------------------------------------------------------------

        mac_get = request
            sa0 <- eth.sa0.read
            sa1 <- eth.sa1.read
            sa2 <- eth.sa2.read

            to8 x = fromInt (toInt x)
            result MACAddress
                (to8 (sa0 .>>. 8))
                (to8 sa0)
                (to8 (sa1 .>>. 8))
                (to8 sa1)
                (to8 (sa2 .>>. 8))
                (to8 sa2)

--------------------------------------------------------------------------------

        mac_set (MACAddress b0 b1 b2 b3 b4 b5) = request
            to32 x = fromInt (toInt x)
            eth.sa0.write (to32 b1 .|. (to32 b0 .<<. 8))
            eth.sa1.write (to32 b3 .|. (to32 b2 .<<. 8))
            eth.sa2.write (to32 b5 .|. (to32 b4 .<<. 8))
            result ()

--------------------------------------------------------------------------------

        transmit buffers = request
            case m_state of
                LinkInitialized ->
                    eth.memory_write buffers
                _           ->
                    result ()

--------------------------------------------------------------------------------

        result Driver {..}

--------------------------------------------------------------------------------

private

--------------------------------------------------------------------------------

    data DriverState =
        Uninitialized   |
        Init            |
        Error String    |
        Initialized     |
        LinkInit        |
        LinkInitialized |
        Shutdown

--------------------------------------------------------------------------------

    interrupt_dispatcher intstatus intclear handler = do
        -- Read status and clear all interrupts.
        status <- intstatus.read
        intclear.write status

        -- Handle the interrupt (hopefully).
        send handler status

        -- All interrupts are "handled".
        result ()

