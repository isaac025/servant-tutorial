module Main where

import Network.Wai.Handler.Warp (run)
import Server (app1, app2, app3, app4)

firstMain :: IO ()
firstMain = run 8081 app1

secondMain :: IO ()
secondMain = run 8081 app2

thirdMain :: IO ()
thirdMain = run 8081 app3

main :: IO ()
main = run 8081 app4
