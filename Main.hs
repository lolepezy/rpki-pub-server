
import Control.Monad
import Happstack.Server ( Method(POST), Response, ServerPart, dir, method, toResponse
                        , nullConf, ok, simpleHTTP, seeOther, path
                        )

main :: IO ()
main = simpleHTTP nullConf $ ok (toResponse "Hello, World!") 

