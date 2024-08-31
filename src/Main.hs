module Main where

import Network.Wai.Handler.Warp (run)
import Server (app1, app10, app2, app3, app4, app5, app6, app7, app8, app9)

firstMain :: IO ()
firstMain = run 8081 app1

secondMain :: IO ()
secondMain = run 8081 app2

thirdMain :: IO ()
thirdMain = run 8081 app3

fourthMain :: IO ()
fourthMain = run 8081 app4

fithMain :: IO ()
fithMain = run 8081 app5

sixthMain :: IO ()
sixthMain = run 8081 app6

seventhMain :: IO ()
seventhMain = run 8081 app7

eigthMain :: IO ()
eigthMain = run 8081 app8

ninthMain :: IO ()
ninthMain = run 8081 app9

main :: IO ()
main = run 8081 app10
