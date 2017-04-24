module Ncurses

import Effects

%lib C "ncurses"
%include C "Ncurses/ncurses_extra.h"
%link C "Ncurses/ncurses_extra.o"

%access public

data Window = WindowPtr Ptr

-- XXX: A good chunk of this effect is copied semi-blindly from the File
-- implementation, and might not be appropriate. In particular, this
-- doesn't bother handling failure: instead, lots of the operations
-- below just return Bool. There's probably a better way of handling
-- failure in the Effect itself (rather than having to check on each
-- individual operation.)
abstract data Ncurses : Effect where
  NewWindow   : {() ==> {res} if res then Window else ()} Ncurses Bool
  NCAction    : (Window -> IO a) -> {Window} Ncurses a
  CloseWindow : {Window ==> ()} Ncurses ()

instance Handler Ncurses IO where
  handle () NewWindow k = do
    p <- foreign FFI_C "initscr" (IO Ptr)
    isNull <- nullPtr p
    if isNull then k False ()
              else k True (WindowPtr p)
  handle w (NCAction f) k = do
    e <- f w
    k e w
  handle w CloseWindow k = do
    foreign FFI_C "endwin" (IO Int)
    k () ()

-- This is also maybe not the best idea: the effect is here called
-- CURSES', but because most of the time we're dealing either with
-- an actively open ncurses session or simply a closed one, there
-- are two aliases for those states called CURSES and CURSES_OFF,
-- respectively. This might be a bad idea?
CURSES' : Type -> EFFECT
CURSES' t = MkEff t Ncurses

CURSES : EFFECT
CURSES = CURSES' Window

CURSES_OFF : EFFECT
CURSES_OFF = CURSES' ()

-- These are used for converting from the C integer-ey representation
-- to actual bools and vice versa!
private
cBool : Bool -> Int
cBool True = 1
cBool False = 0

private
boolC : Int -> Bool
boolC 0 = False
boolC _ = True

--
-- Global Variables
--


lines : {[CURSES]} Eff Int
lines = call (NCAction (const (foreign FFI_C "idr_getLines" (IO Int))))

cols : {[CURSES]} Eff Int
cols = call (NCAction (const (foreign FFI_C "idr_getCols" (IO Int))))

--
-- Window/Screen Management
--

initscr : {[CURSES_OFF] ==> {result}
           [CURSES' (if result
                       then Window
                       else ())]} Eff Bool
initscr = call NewWindow

endwin : {[CURSES] ==> [CURSES_OFF]} Eff ()
endwin = call CloseWindow

--
-- I/O
--

refresh : {[CURSES]} Eff Bool
refresh = call (NCAction go)
  where go (WindowPtr p) = map boolC (foreign FFI_C "wrefresh" (Ptr -> IO Int) p)

getch : {[CURSES]} Eff (Maybe Int)
getch = call (NCAction go)
  where chrFromInt : Int -> Maybe Int
        chrFromInt n = if n < 0 then Nothing else Just n
        go (WindowPtr p) = map chrFromInt (foreign FFI_C "wgetch" (Ptr -> IO Int) p)


putStr : String -> {[CURSES]} Eff ()
putStr s = call (NCAction go)
  where go (WindowPtr p) = map (const ()) (foreign FFI_C "wprintw" (Ptr -> String -> IO Int) p s)

putStrLn : String -> {[CURSES]} Eff ()
putStrLn s = putStr (s ++ "\n")

move : Int -> Int -> {[CURSES]} Eff ()
move y x =  call (NCAction go)
  where go (WindowPtr p) = map (const ()) (foreign FFI_C "wmove" (Ptr -> Int -> Int -> IO Int) p y x)

getYX : {[CURSES]} Eff (Int, Int)
getYX = call (NCAction go)
  where go (WindowPtr p) = do
          x <- foreign FFI_C "idr_getX" (Ptr -> IO Int) p
          y <- foreign FFI_C "idr_getY" (Ptr -> IO Int) p
          pure (y, x)

--
-- Wrapping helpers for curses operations
--

private
boolAction : IO Int -> {[CURSES]} Eff Bool
boolAction f = call (NCAction (const (map boolC f)))

private
nullAction : IO () -> {[CURSES]} Eff ()
nullAction f = call (NCAction (const f))

private
windowSetAction : (Ptr -> Int -> IO Int) -> (Bool -> {[CURSES]} Eff Bool)
windowSetAction f b = call (NCAction go)
  where go (WindowPtr p) = map boolC (f p (cBool b))

private
windowSetAction' : (Ptr -> Int -> IO ()) -> (Bool -> {[CURSES]} Eff ())
windowSetAction' f b = call (NCAction go)
  where go (WindowPtr p) = f p (cBool b)

--
-- Input Options
--

cbreak : {[CURSES]} Eff Bool
cbreak = boolAction (foreign FFI_C "cbreak" (IO Int))

nocbreak : {[CURSES]} Eff Bool
nocbreak = boolAction (foreign FFI_C "nocbreak" (IO Int))

echo : {[CURSES]} Eff Bool
echo = boolAction (foreign FFI_C "echo" (IO Int))

noecho : {[CURSES]} Eff Bool
noecho = boolAction (foreign FFI_C "noecho" (IO Int))

halfdelay : {[CURSES]} Eff Bool
halfdelay = boolAction (foreign FFI_C "halfdelay" (IO Int))

intrflush : Bool -> {[CURSES]} Eff Bool
intrflush = windowSetAction (foreign FFI_C "intrflush" (Ptr -> Int -> IO Int))


keypad : Bool -> {[CURSES]} Eff Bool
keypad = windowSetAction (foreign FFI_C "keypad" (Ptr -> Int -> IO Int))

meta : Bool -> {[CURSES]} Eff Bool
meta = windowSetAction (foreign FFI_C "meta" (Ptr -> Int -> IO Int))

nodelay : Bool -> {[CURSES]} Eff Bool
nodelay = windowSetAction (foreign FFI_C "nodelay" (Ptr -> Int -> IO Int))

raw : {[CURSES]} Eff Bool
raw = boolAction (foreign FFI_C "raw" (IO Int))

noraw : {[CURSES]} Eff Bool
noraw =  boolAction (foreign FFI_C "noraw" (IO Int))

noqiflush : {[CURSES]} Eff ()
noqiflush = nullAction (foreign FFI_C "noqiflush" (IO Unit))

qiflush : {[CURSES]} Eff ()
qiflush = nullAction (foreign FFI_C "qiflush" (IO Unit))

notimeout : Bool -> {[CURSES]} Eff Bool
notimeout = windowSetAction (foreign FFI_C "notimeout" (Ptr -> Int -> IO Int))

timeout : Int -> {[CURSES]} Eff ()
timeout delay = call (NCAction go)
  where go _ = foreign FFI_C "timeout" (Int -> IO Unit) delay

wtimeout : Int -> {[CURSES]} Eff ()
wtimeout delay = call (NCAction go)
  where go (WindowPtr p) =
          foreign FFI_C "wtimeout" (Ptr -> Int -> IO Unit) p delay


-- fd is a file descriptor? What to do...
--typeahead : Int -> IO ()
--typeahead fd =  foreign FFI_C "typeahead" (Int -> IO Int) fd

--
-- Output Options.
--

clearok : Bool -> {[CURSES]} Eff Bool
clearok = windowSetAction (foreign FFI_C "clearok" (Ptr -> Int -> IO Int))

idlok : Bool -> {[CURSES]} Eff Bool
idlok = windowSetAction (foreign FFI_C "idlok" (Ptr -> Int -> IO Int))

idcok : Bool -> {[CURSES]} Eff ()
idcok = windowSetAction' (foreign FFI_C "idcok" (Ptr -> Int -> IO Unit))

immedok : Bool -> {[CURSES]} Eff ()
immedok = windowSetAction' (foreign FFI_C "immedok" (Ptr -> Int -> IO Unit))

leaveok : Bool -> {[CURSES]} Eff Bool
leaveok = windowSetAction (foreign FFI_C "leaveok" (Ptr -> Int -> IO Int))


setscrreg : Int -> Int -> {[CURSES]} Eff Bool
setscrreg top bot = call (NCAction go)
  where go _ = map boolC (foreign FFI_C "setscrreg" (Int -> Int -> IO Int) top bot)

wsetscrreg : Int -> Int -> {[CURSES]} Eff Bool
wsetscrreg top bot = call (NCAction go)
  where go (WindowPtr p) = map boolC $
          foreign FFI_C "wsetscrreg" (Ptr -> Int -> Int -> IO Int) p top bot

scrollok : Bool -> {[CURSES]} Eff Bool
scrollok = windowSetAction (foreign FFI_C "scrollok" (Ptr -> Int -> IO Int))

nl : {[CURSES]} Eff Bool
nl = call (NCAction go)
  where go _ = map boolC (foreign FFI_C "nl" (IO Int))

nonl : {[CURSES]} Eff Bool
nonl = call (NCAction go)
  where go _ = map boolC (foreign FFI_C "nonl" (IO Int))
