import Server
import Web.Scotty (scotty)

main :: IO ()
main = scotty 8000 appRoutes
