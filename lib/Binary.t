module Binary where

    import BitOps

    struct BinBuffer where
        nbytes  :: Int
        words   :: Array BITS32

    binBuf n a              = { nbytes = n, words = a }
    
    totalLength buffers     = foldl (+) 0 (map (.nbytes) buffers)

    emptyBuffer             = binBuf 0 (array [])

--------------------------------------------------------------------------------

    typeclass BinString a where
        pack   :: a -> BinBuffer
        unpack :: BinBuffer -> Int -> Maybe (Int, a)

    instance BinString Int where
        pack n                  = binBuf 4 (array [hton32 (fromInt n)])
        unpack buf off
          | buf.nbytes < off+4  = Nothing
          | otherwise           = Just (4, toInt (ntoh32 (extract32 buf.words off)))
        
    instance BinString Char where
        pack c                  = binBuf 1 (array [fromInt (ord c)])
        unpack buf off
          | buf.nbytes < off+1  = Nothing
          | otherwise           = Just (1, chr (toInt (extract8 buf.words off)))

    instance BinString BITS8 where
        pack b                  = binBuf 1 (array [to32 b])
        unpack buf off
          | buf.nbytes < off+1  = Nothing
          | otherwise           = Just (1, extract8 buf.words off)

    instance BinString BITS16 where
        pack b                  = binBuf 2 (array [to32 (hton16 b)])
        unpack buf off
          | buf.nbytes < off+2  = Nothing
          | otherwise           = Just (2, ntoh16 (extract16 buf.words off))

    instance BinString BITS32 where
        pack b                  = binBuf 4 (array [hton32 b])
        unpack buf off
          | buf.nbytes < off+4  = Nothing
          | otherwise           = Just (4, ntoh32 (extract32 buf.words off))

    instance BinString [Char] where
        pack s                  = binBuf (length s) (array (wordlist s))
        unpack buf off          = Just (buf.nbytes - off, charlist buf off)

    wordlist []                 = []
    wordlist (c0:c1:c2:c3:s)    = (b0 .|. b1 .|. b2 .|. b3) : wordlist s
      where b0                  = fromInt (ord c0)
            b1                  = fromInt (ord c1) .<<.8
            b2                  = fromInt (ord c2) .<<. 16
            b3                  = fromInt (ord c3) .<<. 24
    wordlist s                  = wordlist (s ++ "\0")
    
    charlist buf off
      | buf.nbytes < off        = []
      | otherwise               = chr (toInt (extract8 buf.words off)) : charlist buf (off+1)
    
--------------------------------------------------------------------------------

    extract8 :: Array BITS32 -> Int -> BITS8
    extract8 words off = fromInt (toInt (extr words (off `div` 4) (off `mod` 4)))
        where
            extr words off m
                | m == 0 = (words!off)
                | m == 1 = (words!off .>>. 8)
                | m == 2 = (words!off .>>. 16)
                | m == 3 = (words!off .>>. 24)

--------------------------------------------------------------------------------

    extract16 :: Array BITS32 -> Int -> BITS16
    extract16 words off = fromInt (toInt (extr words (off `div` 4) (off `mod` 4)))
        where
            extr words off m
                | m == 0 = (words!off)
                | m == 1 = (words!off .>>. 8)
                | m == 2 = (words!off .>>. 16)
                | m == 3 = (words!off .>>. 24) .|. (words!(off+1) .<<. 8)

--------------------------------------------------------------------------------

    extract32 :: Array BITS32 -> Int -> BITS32
    extract32 words off = extr words (off `div` 4) (off `mod` 4)
        where
            extr words off m
                | m == 0 = (words!off)
                | m == 1 = (words!off .>>. 8)  .|. (words!(off+1) .<<. 24)
                | m == 2 = (words!off .>>. 16) .|. (words!(off+1) .<<. 16)
                | m == 3 = (words!off .>>. 24) .|. (words!(off+1) .<<. 8)

--------------------------------------------------------------------------------

    hton16 :: BITS16 -> BITS16
    hton16 bits =
        ((bits .>>. 8) .|. (bits.<<. 8))

--------------------------------------------------------------------------------

    ntoh16 :: BITS16 -> BITS16
    ntoh16 bits = hton16 bits

--------------------------------------------------------------------------------

    hton32 :: BITS32 -> BITS32
    hton32 bits =
        (
            (bits .>>. 24) .|.
            ((bits .>>. 8) .&. 0x0000ff00) .|.
            ((bits .<<. 8) .&. 0x00ff0000) .|.
            (bits .<<. 24)
        )

--------------------------------------------------------------------------------

    ntoh32 :: BITS32 -> BITS32
    ntoh32 long = hton32 long

--------------------------------------------------------------------------------

    to8 :: _ -> BITS8
    to8 bits = fromInt (toInt bits)::BITS8

--------------------------------------------------------------------------------

    to16 :: _ -> BITS16
    to16 bits = fromInt (toInt bits)::BITS16

--------------------------------------------------------------------------------

    to32 :: _ -> BITS32
    to32 bits = (fromInt (toInt bits))::BITS32

--------------------------------------------------------------------------------

    smask32 :: BITS32 -> Int-> BITS32 -> BITS32
    smask32 bits shift mask = (bits .>>. shift) .&. mask

--------------------------------------------------------------------------------

    smask16 :: BITS16 -> Int-> BITS16 -> BITS16
    smask16 bits shift mask = (bits .>>. shift) .&. mask

--------------------------------------------------------------------------------

    checksumCore :: Array BITS32 -> Int -> Int -> BITS16
    checksumCore words off len = fromInt (cs_fold (sum 0))
      where sum n = if len-n <= 0 then 0
                    else if len-n == 1 then toInt (extract8 words (off + n))
                    else toInt (extract16 words (off + n)) + sum (n + 2)

--------------------------------------------------------------------------------

    cs_add :: BITS16 -> BITS16 -> BITS16
    cs_add c0 c1 = fromInt (cs_fold csum)
      where csum = toInt c0 + toInt c1

--------------------------------------------------------------------------------

    checksum :: BinBuffer -> BITS16
    checksum buf = checksumCore buf.words 0 buf.nbytes
    
    checksums :: [BinBuffer] -> BITS16
    checksums [] = 0
    checksums (buf:bufs) = cs_add (checksum buf) (checksums bufs)
    
--------------------------------------------------------------------------------

    instance showArrayBITS32 :: Show (Array BITS32) where
        show arr
            | (size arr) == 0 = "[]"
            | otherwise       = "[" ++ show_array arr 0
            where
                show_array arr n
                    | (n + 1) == (size arr) = (showhex (arr!n)) ++ "]"
                    | otherwise       = (showhex (arr!n)) ++ "," ++ (show_array arr (n + 1))

--------------------------------------------------------------------------------

private

    -- Used to "fold" the checksum into 16 bits.
    cs_fold csum
      | csum > 0xffff   = cs_fold (upper + lower)
      where upper       = toInt (((fromInt csum)::BITS32) .>>. 16)
            lower       = toInt (((fromInt csum)::BITS32) .&. 0xffff)
    cs_fold csum        = csum
