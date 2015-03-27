module Main

import Effects
import Ncurses

loop : {[CURSES]} Eff ()
loop = do
  x <- getch
  case x of
    Just 27  => pure ()
    Just 81  => pure ()
    Just 113 => pure ()
    _        => do
      putStrLn ("Pressed <" ++ show x ++ ">")
      (y, x) <- getYX
      move (y - 1) x
      loop

setup : {[CURSES]} Eff ()
setup = do
  cbreak
  noecho
  intrflush False
  ln <- lines
  cl <- cols
  putStrLn (show ln ++ " x " ++ show cl)
  putStrLn "Press q or ESC to exit."

cursesMain : {[CURSES_OFF]} Eff ()
cursesMain = do
  s <- initscr
  case s of
    True => do
      setup
      loop
      endwin
    False => pure ()

main : IO ()
main = run cursesMain
