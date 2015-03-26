module Ncurses

%lib C "ncurses"
%include C "Ncurses/ncurses_extra.h"
%link C "Ncurses/ncurses_extra.o"

%access public

data NcursesError = NullWindow

data NcursesIO : Type -> Type where
  ncursesIO : IO (Either NcursesError a) -> NcursesIO a

unliftIO : NcursesIO a -> IO (Either NcursesError a)
unliftIO (ncursesIO ioe) = ioe

liftIO : IO a -> NcursesIO a
liftIO ioa = ncursesIO (map Right ioa)

instance Functor NcursesIO where
  map f (ncursesIO io) = ncursesIO (map (map f) io)

instance Applicative NcursesIO where
  pure a = ncursesIO (pure (pure a))
  (ncursesIO f) <*> (ncursesIO a) = ncursesIO io where
    io : IO (Either NcursesError b)
    io = do
      f' <- f
      a' <- a
      return (f' <*> a')

instance Monad NcursesIO where
  (ncursesIO a) >>= k = ncursesIO io where
    io : IO (Either NcursesError b)
    io = do
      a' <- a
      case a' of
        Left err => return (Left err)
        Right a => unliftIO (k a)

data Window = WindowPtr Ptr

private
cBool : Bool -> Int
cBool True = 1
cBool False = 0

private
liftError : IO Int -> NcursesIO ()
liftError code = ncursesIO (map liftErr code) where
  liftErr code = if code == -1 then Left NullWindow else Right ()

--
-- Global Variables
--

lines : NcursesIO Int
lines = liftIO $ foreign FFI_C "getLines" (IO Int)

cols : NcursesIO Int
cols = liftIO $ foreign FFI_C "getCols" (IO Int)

--
-- Window/Screen Management
--

initscr : NcursesIO Window
initscr = liftIO $ map WindowPtr $ foreign FFI_C "initscr" (IO Ptr)

endwin : NcursesIO ()
endwin = liftError $ foreign FFI_C "endwin" (IO Int)

--
-- I/O
--

refresh : Window -> NcursesIO ()
refresh (WindowPtr p) = liftError $ foreign FFI_C "wrefresh" (Ptr -> IO Int) p

getch : Window -> NcursesIO (Maybe Int)
getch (WindowPtr p) = liftIO (map go (foreign FFI_C "wgetch" (Ptr -> IO Int) p))
  where go chr = if chr == -1 then Nothing else Just chr

putStr : Window -> String -> NcursesIO ()
putStr (WindowPtr ptr) s = liftError $ foreign FFI_C "wprintw" (Ptr -> String -> IO Int) ptr s

putStrLn : Window -> String -> NcursesIO ()
putStrLn w s = putStr w (s ++ "\n")

move : Window -> Int -> Int -> NcursesIO ()
move (WindowPtr p) y x = liftError $ foreign FFI_C "wmove" (Ptr -> Int -> Int -> IO Int) p y x

--
-- Input Options
--

cbreak : NcursesIO ()
cbreak = liftError $ foreign FFI_C "cbreak" (IO Int)

nocbreak : NcursesIO ()
nocbreak = liftError $ foreign FFI_C "nocbreak" (IO Int)

echo : NcursesIO ()
echo = liftError $ foreign FFI_C "echo" (IO Int)

noecho : NcursesIO ()
noecho = liftError $ foreign FFI_C "noecho" (IO Int)

halfdelay : NcursesIO ()
halfdelay = liftError $ foreign FFI_C "halfdelay" (IO Int)

intrflush : Window -> Bool -> NcursesIO ()
intrflush (WindowPtr ptr) p =
  liftError $ foreign FFI_C "intrflush" (Ptr -> Int -> IO Int) ptr (cBool p)

keypad : Window -> Bool -> NcursesIO ()
keypad (WindowPtr ptr) p =
  liftError $ foreign FFI_C "keypad" (Ptr -> Int -> IO Int) ptr (cBool p)

meta : Window -> Bool -> NcursesIO ()
meta (WindowPtr ptr) p =
  liftError $ foreign FFI_C "meta" (Ptr -> Int -> IO Int) ptr (cBool p)

nodelay : Window -> Bool -> NcursesIO ()
nodelay (WindowPtr ptr) p =
  liftError $ foreign FFI_C "nodelay" (Ptr -> Int -> IO Int) ptr (cBool p)

raw : NcursesIO ()
raw = liftError $ foreign FFI_C "raw" (IO Int)

noraw : NcursesIO ()
noraw = liftError $ foreign FFI_C "noraw" (IO Int)

noqiflush : NcursesIO ()
noqiflush = liftIO $ foreign FFI_C "noqiflush" (IO Unit)

qiflush : NcursesIO ()
qiflush = liftIO $ foreign FFI_C "qiflush" (IO Unit)

notimeout : Window -> Bool -> NcursesIO ()
notimeout (WindowPtr ptr) p =
  liftError $ foreign FFI_C "notimeout" (Ptr -> Int -> IO Int) ptr (cBool p)

timeout : Int -> NcursesIO ()
timeout delay = liftIO $ foreign FFI_C "timeout" (Int -> IO Unit) delay

wtimeout : Window -> Int -> NcursesIO ()
wtimeout (WindowPtr ptr) delay =
  liftIO $ foreign FFI_C "wtimeout" (Ptr -> Int -> IO Unit) ptr delay

-- fd is a file descriptor? What to do...
typeahead : Int -> NcursesIO ()
typeahead fd = liftError $ foreign FFI_C "typeahead" (Int -> IO Int) fd

--
-- Output Options.
--

clearok : Window -> Bool -> NcursesIO ()
clearok (WindowPtr ptr) p =
  liftError $ foreign FFI_C "clearok" (Ptr -> Int -> IO Int) ptr (cBool p)

idlok : Window -> Bool -> NcursesIO ()
idlok (WindowPtr ptr) p =
  liftError $ foreign FFI_C "idlok" (Ptr -> Int -> IO Int) ptr (cBool p)

idcok : Window -> Bool -> NcursesIO ()
idcok (WindowPtr ptr) p =
  liftIO $ foreign FFI_C "idcok" (Ptr -> Int -> IO Unit) ptr (cBool p)

immedok : Window -> Bool -> NcursesIO ()
immedok (WindowPtr ptr) p =
  liftIO $ foreign FFI_C "immedok" (Ptr -> Int -> IO Unit) ptr (cBool p)

leaveok : Window -> Bool -> NcursesIO ()
leaveok (WindowPtr ptr) p =
  liftError $ foreign FFI_C "leaveok" (Ptr -> Int -> IO Int) ptr (cBool p)

setscrreg : Int -> Int -> NcursesIO ()
setscrreg top bot =
  liftError $ foreign FFI_C "setscrreg" (Int -> Int -> IO Int) top bot

wsetscrreg : Window -> Int -> Int -> NcursesIO ()
wsetscrreg (WindowPtr ptr) top bot =
  liftError $ foreign FFI_C "wsetscrreg" (Ptr -> Int -> Int -> IO Int) ptr top bot

scrollok : Window -> Bool -> NcursesIO ()
scrollok (WindowPtr ptr) p =
  liftError $ foreign FFI_C "scrollok" (Ptr -> Int -> IO Int) ptr (cBool p)

nl : NcursesIO ()
nl = liftError $ foreign FFI_C "nl" (IO Int)

nonl : NcursesIO ()
nonl = liftError $ foreign FFI_C "nonl" (IO Int)

runNcurses : (NcursesError -> IO b) -> (a -> IO b) -> NcursesIO a -> IO b
runNcurses f g nio = do
  e <- unliftIO nio
  case e of
    Left err => f err
    Right a => g a

ncursesMain : NcursesIO () -> IO ()
ncursesMain = runNcurses logError return where
  logError NullWindow = putStrLn "NULL window"
