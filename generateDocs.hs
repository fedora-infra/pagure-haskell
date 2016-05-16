import Web.Pagure
import Servant.Docs

main :: IO ()
main = putStrLn $ markdown (docs api)
