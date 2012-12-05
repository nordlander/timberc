module LPC24XXEthernetPort where

import ARM
import Port
import Binary

--------------------------------------------------------------------------------

struct Ethernet where
    mac1            :: IOPort
    mac2            :: IOPort
    ipgt            :: IOPort
    ipgr            :: IOPort
    clrt            :: IOPort
    maxf            :: IOPort
    supp            :: IOPort
    test            :: IOPort
    mcfg            :: IOPort
    mcmd            :: IOPort
    madr            :: IOPort
    mwtd            :: OPort
    mrdd            :: IPort
    mind            :: IPort

    sa0             :: IOPort
    sa1             :: IOPort
    sa2             :: IOPort
    
    command         :: IOPort
    status          :: IPort

    rxdescriptor    :: IOPort
    rxstatus        :: IOPort
    rxdescriptornum :: IOPort
    rxproduceindex  :: IPort
    rxconsumeindex  :: IOPort

    txdescriptor    :: IOPort
    txstatus        :: IOPort
    txdescriptornum :: IOPort
    txproduceindex  :: IOPort
    txconsumeindex  :: IPort

    tsv0            :: IPort
    tsv1            :: IPort
    rsv             :: IPort

    flowcontrolcnt  :: IOPort
    flowcontrolsts  :: IPort

    rxfilterctrl    :: IOPort
    rxfilterwolsts  :: IOPort
    rxfilterwolclr  :: IOPort

    hashfilterl     :: IOPort
    hashfilterh     :: IOPort

    intstatus       :: IPort
    intenable       :: IOPort
    intclear        :: OPort
    intset          :: OPort

    powerdown       :: IOPort
    moduleid        :: IOPort

    poweron         :: Request ()
    reset           :: Action -> Request ()
    interrupt       :: Cmd () a -> Request ()
    memory_init     :: Request ()
    memory_read     :: Request BinBuffer
    memory_write    :: [BinBuffer] -> Request ()
    poweroff        :: Request ()

    phy_addr        :: BITS32

--------------------------------------------------------------------------------

ethernetPort :: Env -> Addr -> Class Ethernet
ethernetPort env base_addr = class

--------------------------------------------------------------------------------

    m_drop          := False

--------------------------------------------------------------------------------

    mac1            = new ioport env (fromInt (base_addr + 0x000))
    mac2            = new ioport env (fromInt (base_addr + 0x004))
    ipgt            = new ioport env (fromInt (base_addr + 0x008))
    ipgr            = new ioport env (fromInt (base_addr + 0x00c))
    clrt            = new ioport env (fromInt (base_addr + 0x010))
    maxf            = new ioport env (fromInt (base_addr + 0x014))
    supp            = new ioport env (fromInt (base_addr + 0x018))
    test            = new ioport env (fromInt (base_addr + 0x01c))
    mcfg            = new ioport env (fromInt (base_addr + 0x020))
    mcmd            = new ioport env (fromInt (base_addr + 0x024))
    madr            = new ioport env (fromInt (base_addr + 0x028))
    mwtd            = new oport  env (fromInt (base_addr + 0x02c))
    mrdd            = new iport  env (fromInt (base_addr + 0x030))
    mind            = new iport  env (fromInt (base_addr + 0x034))

    sa0             = new ioport env (fromInt (base_addr + 0x040))
    sa1             = new ioport env (fromInt (base_addr + 0x044))
    sa2             = new ioport env (fromInt (base_addr + 0x048))
    
    command         = new ioport env (fromInt (base_addr + 0x100))
    status          = new iport  env (fromInt (base_addr + 0x104))

    rxdescriptor    = new ioport env (fromInt (base_addr + 0x108))
    rxstatus        = new ioport env (fromInt (base_addr + 0x10c))
    rxdescriptornum = new ioport env (fromInt (base_addr + 0x110))
    rxproduceindex  = new iport  env (fromInt (base_addr + 0x114))
    rxconsumeindex  = new ioport env (fromInt (base_addr + 0x118))

    txdescriptor    = new ioport env (fromInt (base_addr + 0x11c))
    txstatus        = new ioport env (fromInt (base_addr + 0x120))
    txdescriptornum = new ioport env (fromInt (base_addr + 0x124))
    txproduceindex  = new ioport env (fromInt (base_addr + 0x128))
    txconsumeindex  = new iport  env (fromInt (base_addr + 0x12c))

    tsv0            = new iport  env (fromInt (base_addr + 0x158))
    tsv1            = new iport  env (fromInt (base_addr + 0x15c))
    rsv             = new iport  env (fromInt (base_addr + 0x160))

    flowcontrolcnt  = new ioport env (fromInt (base_addr + 0x170))
    flowcontrolsts  = new iport  env (fromInt (base_addr + 0x174))

    rxfilterctrl    = new ioport env (fromInt (base_addr + 0x200))
    rxfilterwolsts  = new ioport env (fromInt (base_addr + 0x204))
    rxfilterwolclr  = new ioport env (fromInt (base_addr + 0x208))

    hashfilterl     = new ioport env (fromInt (base_addr + 0x210))
    hashfilterh     = new ioport env (fromInt (base_addr + 0x214))

    intstatus       = new iport  env (fromInt (base_addr + 0xfe0))
    intenable       = new ioport env (fromInt (base_addr + 0xfe4))
    intclear        = new oport  env (fromInt (base_addr + 0xfe8))
    intset          = new oport  env (fromInt (base_addr + 0xfec))

    powerdown       = new ioport env (fromInt (base_addr + 0xff4))
    moduleid        = new ioport env (fromInt (base_addr + 0xffc))

--------------------------------------------------------------------------------

    poweron = request
        -- Enable power, ugly hack for now...
        pcop = new ioport env (fromInt 0xe01fc0c4)
        pcop.set 0x40000000

        -- Setup the pins.
        pinsel2 = new ioport env (fromInt 0xe002c008)
        pinsel3 = new ioport env (fromInt 0xe002c00c)

        -- Setup the ports/pins.
        id <- moduleid.read
        if id == (fromInt 0x39022000) then
            -- Old module id (Revision A.)
            pinsel2.write 0x50151105
        else
            -- Newer revisions.
            pinsel2.set 0x50150105
        pinsel3.set 0x00000005

        result ()

--------------------------------------------------------------------------------

    reset done_cb = request

        -- Reset mac modules, tc, mcs_tx, rx, mcs_rx, simulation, and soft reset
        mac1.write 0xcf00

        -- Reset data path and host registers
        command.write 0x38

        -- We should hopefully be done after 100 ms
        after millisec 100 send action
            -- Clear mac1 to disable tx, rx etc.
            mac1.write 0

            -- Setup some default values to offload the driver.
            -- These values are all taken from the manual.
            ipgr.write 0x0c12
            ipgt.write 0x12

            done_cb
        result ()

--------------------------------------------------------------------------------

    -- Some info about the memory we should use.
    memory_addr = 0x7fe00000
    memory_size = 0x00004000

    -- The blocksize and the number of blocks.
    block_size = 0x600
    tx_blocks = 5
    rx_blocks = 5
    
    -- Tx descriptor size and status size.
    txd_size = tx_blocks * 8
    txs_size = tx_blocks * 4

    -- Rx descriptor size and staus size.
    rxd_size = rx_blocks * 8
    rxs_size = rx_blocks * 8

    -- Total descriptor size.
    descriptor_size = rxd_size + txd_size + rxs_size + txs_size

    -- Tx descriptor and status address.
    txd_addr = memory_addr + memory_size - descriptor_size
    txs_addr = txd_addr + txd_size

    -- Rx descriptor and status address.
    rxd_addr = txs_addr + txs_size
    rxs_addr = rxd_addr + rxd_size

    -- Tx and Rx buffer addresses.
    txb_addr = memory_addr
    rxb_addr = memory_addr + (tx_blocks * block_size)
    
--------------------------------------------------------------------------------

    memory_init = request

{-
        env.debug ("txd_size:        " ++ (showhex ((fromInt txd_size)::BITS32)) ++ "\r\n")
        env.debug ("txs_size:        " ++ (showhex ((fromInt txs_size)::BITS32)) ++ "\r\n")

        env.debug ("rxd_size:        " ++ (showhex ((fromInt rxd_size)::BITS32)) ++ "\r\n")
        env.debug ("rxs_size:        " ++ (showhex ((fromInt rxs_size)::BITS32)) ++ "\r\n")

        env.debug ("descriptor_size: " ++ (showhex ((fromInt descriptor_size)::BITS32)) ++ "\r\n")

        env.debug ("txd_addr:        " ++ (showhex ((fromInt txd_addr)::BITS32)) ++ "\r\n")
        env.debug ("txs_addr:        " ++ (showhex ((fromInt txs_addr)::BITS32)) ++ "\r\n")

        env.debug ("rxd_addr:        " ++ (showhex ((fromInt rxd_addr)::BITS32)) ++ "\r\n")
        env.debug ("rxs_addr:        " ++ (showhex ((fromInt rxs_addr)::BITS32)) ++ "\r\n")

        env.debug ("txb_addr:        " ++ (showhex ((fromInt txb_addr)::BITS32)) ++ "\r\n")
        env.debug ("rxb_addr:        " ++ (showhex ((fromInt rxb_addr)::BITS32)) ++ "\r\n")
-}

        -- Write the Tx buffer, descriptor, and status addresses and count.
        txdescriptor.write (fromInt txd_addr)
        --txdescriptor.dump "txdescriptor"
        txstatus.write (fromInt txs_addr)
        --txstatus.dump "txstatus"
        txdescriptornum.write (fromInt (tx_blocks - 1))
        --txdescriptornum.dump "txdescriptornum"
        forall i <- [0..(tx_blocks - 1)] do
            -- Write the address of the buffer.
            desc_addr = txd_addr + i * 8
            desc = txb_addr + i * block_size
            env.portwrite desc_addr (fromInt desc)

            -- Write the interrupt bit and the size.
            control = (1 .<<. 31) .|. (fromInt (block_size - 1))
            env.portwrite (desc_addr + 4) control

            -- Write the status (0).
            status_addr = txs_addr + (i * 4)
            env.portwrite status_addr (fromInt 0)
        txproduceindex.write 0
        --txproduceindex.dump "txproduceindex"

        -- Write the Rx buffer, descriptor and status addresses and count.
        rxdescriptor.write (fromInt rxd_addr)
        --rxdescriptor.dump "rxdescriptor"
        rxstatus.write (fromInt rxs_addr)
        --rxstatus.dump "rxstatus"
        rxdescriptornum.write (fromInt (rx_blocks - 1))
        --rxdescriptornum.dump "rxdescriptornum"
        forall i <- [0..(rx_blocks - 1)] do
            -- Write the address of the buffer.
            desc_addr = rxd_addr + i * 8
            desc = rxb_addr + i * block_size
            env.portwrite desc_addr (fromInt desc)
            --env.debug ("rx-block:        " ++ (showhex ((fromInt desc)::BITS32)) ++ "\r\n")

            -- Write the interrupt bit and size
            control = (1 .<<. 31) .|. (fromInt (block_size - 1))
            env.portwrite (desc_addr + 4) control
            --env.debug ("rx-ctrl:         " ++ (showhex control) ++ "\r\n")

            -- Write the status (0), we have 2 words in the rx status.
            status_addr = rxs_addr + (i * 8)
            env.portwrite status_addr (fromInt 0)
            env.portwrite (status_addr + 4) (fromInt 0)
            --env.debug ("rx-status:       " ++ (showhex ((fromInt status_addr)::BITS32)) ++ "\r\n")
        rxconsumeindex.write 0
        --rxconsumeindex.dump "rxconsumeindex"

        -- Buffers/Descriptors initialized.
        result ()


--------------------------------------------------------------------------------

    memory_read = request
        
        -- Shorthand to avoid excessive crap.
        one = 1::BITS32
        zero = 0::BITS32

        -- Just for sanity, try not to drop anything.
        produce <- rxproduceindex.read

        -- Read out some data information.
        desc <- rxconsumeindex.read
        desc_addr <- rxdescriptor.read
        status_addr <- rxstatus.read

        -- Grab the packet address and status address.
        packet <- env.portread ((toInt desc_addr) + 8 * (toInt desc))
        status <- env.portread ((toInt status_addr) + 8 * (toInt desc))

        -- Get the buffer address.
        buf_addr = (toInt packet)
        
        -- Get the size of the buffer.
        buf_size = (toInt (status .&. 0x3ff))

        -- Grab the errors, ignore the range-error (for a good reason).
        buf_errors = (status .&. (0x3f .<<. 23)) .&. (binv (one .<<. 26))

        -- Grab the "last" flag.
        buf_last = status .&. (one .<<. 30)

        -- Check if the frame was a control frame.
        buf_control = status .&. (one .<<. 18)

        -- Helper to free a single buffer.
        buf_free = do
            -- Free the buffer for the hardware to use.
            new_desc = ((toInt desc) + 1) `mod` rx_blocks
            rxconsumeindex.write ((fromInt new_desc)::BITS32)

        -- Make sure we have a buffer.
        if desc == produce then
            result emptyBuffer
        -- If there are errors then we should drop things.
        elsif buf_control /= zero then
            buf_free
            --before (sec 5) action env.debug ("cf: " ++ (showhex status) ++ "\r\n")
            result emptyBuffer
        else
            if (buf_errors /= zero) || (buf_last == zero) then
                m_drop := True

            -- Ok, a valid frame, might still want to drop it.
            if m_drop then

                buf_free
                --before (sec 5) action env.debug ("dp: " ++ (showhex status) ++ "\r\n")
                -- Check if this is the last packet
                if (buf_last /= zero) || (buf_control /= zero) then
                    m_drop := False
                result emptyBuffer
            else
                -- Copy the buffer.
                words <- env.netmemread buf_addr buf_size
                buf_free
                result binBuf buf_size words

--------------------------------------------------------------------------------

    write_bufs addr [] = do 
    write_bufs addr (buf:bufs) = do
        env.netmemwrite buf.words addr buf.nbytes
        write_bufs (addr+buf.nbytes) bufs

    memory_write buffers = request
        -- Figure out if we have any write descriptor left.
        tpib <- txproduceindex.read
        tpi = toInt tpib
        tcib <- txconsumeindex.read
        tci = toInt tcib
        totlen = totalLength buffers
        if totlen <= block_size && (tci-1-tpi) `mod` tx_blocks /= tci then

            one = 1::BITS32

            -- We have at least one buffer.
            base_addr <- txdescriptor.read
            addr_buf = (toInt base_addr) + tpi * 8
            addr_status = addr_buf + 4

            -- Fill the dma buffer and update the status word.
            buf_addr <- env.portread addr_buf
            write_bufs (toInt buf_addr) buffers

            bb <- env.netmemread (toInt buf_addr) totlen
            -- env.debug ("netmemwrite (" ++ show totlen ++ "): " ++ show bb ++ "\n")

            status = (fromInt totlen) .|. (one .<<. 28) .|. (one .<<. 29) .|. (one .<<. 30)
            env.portwrite addr_status status

            -- Make sure the hardware knows it should send the buffer.
            ntpi = (tpi + 1) `mod` tx_blocks
            txproduceindex.write (fromInt ntpi)

--------------------------------------------------------------------------------

    interrupt handler = request
        
        -- Setup the handler.
        env.install 21 handler
        result ()

--------------------------------------------------------------------------------

    poweroff        = request
        pcop = new ioport env (fromInt 0xe01fc0c4)
        pcop.clear 0x40000000
        result ()

--------------------------------------------------------------------------------

    -- Default address configuration AD0 pull-up, AD1-4 pull-down
    phy_addr        = (fromInt 0x00001) .<<. 8

--------------------------------------------------------------------------------

    result Ethernet {..}
