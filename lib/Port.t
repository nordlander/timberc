module Port where

    import ARM
    import BitOps

--------------------------------------------------------------------------------

    struct IPort where
        read  :: Request (BITS32)
        dump  :: String -> Request ()

--------------------------------------------------------------------------------

    struct OPort where
        write :: BITS32 -> Request ()

--------------------------------------------------------------------------------

    struct IOPort < IPort, OPort where
        clear :: BITS32 -> Request ()
        set   :: BITS32 -> Request ()

--------------------------------------------------------------------------------

    iport :: Env -> Int -> Class IPort
    iport env address = class
        read = request
            bits <- env.portread address
            result bits

        dump str = request
            bits <- env.portread address
            env.debug (str ++ "(" ++ (showhex ((fromInt address)::BITS32)) ++ "): " ++ (showhex bits) ++ "\r\n")
        result IPort {..}

--------------------------------------------------------------------------------

    oport :: Env -> Int -> Class OPort
    oport env address = class
        write bits = request
            env.portwrite address bits
        result OPort {..}

--------------------------------------------------------------------------------

    ioport env address = class
        input  = new iport env address
        output = new oport env address

        clear bits = request
            tmp <- input.read
            output.write ((binv bits) .&. tmp)

        set bits = request
            tmp <- input.read
            output.write (bits .|. tmp)

        result IOPort {
            read  = input.read,
            dump  = input.dump,
            write = output.write,
            ..
        }

--------------------------------------------------------------------------------

    poller ::
        (String -> Request ()) ->
        IPort ->
        (BITS32 -> Bool) ->
        Time ->
        Time ->
        Action ->
        (BITS32 -> Action) ->
        Class (Request ()) 
    poller debug port cond int until fail_cb done_cb = class

        m_state := Idle

        m_poll = before int action
            bits <- port.read
            if cond bits then
                -- If the condition is true then we are done.
                abort (fail_r m_state)
                send done_cb bits 
            else
                -- Otherwise we try again, we will be cancelled
                -- by the fail action if we reach the timeout.
                poll = after int send m_poll
                fail = fail_r m_state
                m_state := Polling poll fail
            result ()

        result request
            poll = send m_poll
            fail = after until send action
                abort (poll_r m_state)
                send fail_cb
            m_state := Polling poll fail
            result ()

private

--------------------------------------------------------------------------------

    data PollState = Idle | Polling Msg Msg
    poll_r (Polling poll fail) = poll
    fail_r (Polling poll fail) = fail
