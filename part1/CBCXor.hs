import qualified Data.ByteString as B (unpack, pack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Word (Word8)
import Data.Bits

main = do
  let file = "input.txt"
  content <- readFile file
  let (first_block, encrypted) = parseInput content
  let m = recoverMessage first_block encrypted
  putStrLn $ "Recovered message: " ++ show m

unHex :: String -> [Word8]
unHex [] = []
unHex (x:y:xs) = toEnum (read ['0','x',x,y]) : unHex xs

-- | Parses the problem.
parseInput :: String -> ([Word8], [Word8])
parseInput content =
  let fileLines = lines content
      first_block = B.unpack (BC.pack (fileLines !! 0))
      encrypted = unHex (fileLines !! 1)
  in (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form IV | C0 | C1 | ... where each block is 12 bytes
-- long.
recoverMessage :: [Word8] -> [Word8] -> String
recoverMessage first_block encrypted = BC.unpack $ B.pack $ recoverAll (getBlock encrypted 1) (getBlock encrypted 0) (encrypted) (recoverKey encrypted first_block) (1)

recoverAll :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> Int -> [Word8]
recoverAll cb pb e k 11 = []
recoverAll cb pb e k n = (Main.xor (pb) (Main.xor (k) (cb))) ++ (recoverAll (getBlock e (n+1)) (getBlock e n) (e) (k) (n+1)) 
 
recoverKey :: [Word8] -> [Word8] -> [Word8]
recoverKey e fb = Main.xor (fb) (Main.xor (getBlock e 0) (getBlock e 1))

getBlock :: [Word8] -> Int -> [Word8]
getBlock ls n = take (12) $ drop (12*n) ls

xor :: [Word8] -> [Word8] -> [Word8]
xor w1 w2 = zipWith Data.Bits.xor w1 w2