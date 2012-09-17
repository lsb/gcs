import Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Base64.Lazy as B64

main = B.interact B64.encode
