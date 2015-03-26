module Main

import Ncurses

loop : Window -> NcursesIO ()
loop w = do
  x <- getch w
  case x of
    Just 27  => endwin
    Just 81  => endwin
    Just 113 => endwin
    _        => do
      putStrLn w ("Pressed <" ++ show x ++ ">")
      (y, x) <- getYX w
      move w (y - 1) x
      loop w

main : IO ()
main = ncursesMain $ do
  window <- initscr
  cbreak
  noecho
  intrflush window False
  ln <- lines
  cl <- cols
  putStrLn window $ (show ln) ++ " x " ++ (show cl)
  putStrLn window "Press q or ESC to exit."
  loop window
