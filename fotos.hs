{-# LANGUAGE NoMonomorphismRestriction, CPP #-}
import Transient.Internals
import Transient.Move

import GHCJS.HPlay.View hiding (map,head)
import Data.String
import Data.Function ((&))
import Data.List
import Data.Maybe
import Prelude hiding (div,id)
import Data.IORef
import Control.Monad.IO.Class
import Data.Typeable
import Control.Monad(when)
import System.Directory

type Projects = [(String,String,[String])]

projects=
#include "content"
   :: Projects
--projects= [("landscapes","",
--               ["http://agocorona.github.io/roadmap-to-landscapes-finance.jpg"
--               ,"http://feelgrafix.com/data/landscape/landscape-15.jpg"
--              ,"https://upload.wikimedia.org/wikipedia/commons/e/e4/Stourhead_garden.jpg"]),
--
--           ("animals",["http://kids.nationalgeographic.com/content/dam/kids/photos/games/screen-shots/More%20Games/A-G/babyanimal_open.jpg"
--           ,"http://r.ddmcdn.com/w_830/s_f/o_1/cx_98/cy_0/cw_640/ch_360/APL/uploads/2015/07/cecil-AP463227356214-1000x400.jpg"])] :: Projects

type Style=  String
type Project = Int
type Photo= Int
data Current= Current (Project,Photo, Style)

newtype Ref a = Ref (IORef a)

-- | An state reference that can be updated (similar to STRef in the state monad)
--
-- Initialized the first time it is set.
setRData:: Typeable a => a -> TransIO ()
setRData x= do
     Ref ref <- getSData
     liftIO $ atomicModifyIORef ref $ const (x,())
   <|> do
     ref <- liftIO (newIORef x)
     setData $ Ref ref

getRData :: Typeable a => TransIO a
getRData= do
    Ref ref <- getSData
    liftIO $ readIORef ref


files = "files"


filterdir= return . filter ((/= '.') . head)

fs= fromString
main=  do
  -- TODO poner panel de fotos peque¤as
#ifndef ghcjs_HOST_OS
  -- Just generates the Ccontent" file
  prs <- liftIO $ getDirectoryContents files >>= filterdir
  projects <- liftIO $ mapM (\f -> (getDirectoryContents $ files ++ "/" ++ f) >>= filterdir)  prs
  liftIO $ writeFile "content" $ "\t"++ (show $ zip3 (reverse prs) "" projects)

#endif
  runBody $ Widget $  do
   setRData $ Current (0,0,"")
   liftIO $ addHeader $ link ! atr (fs "rel") (fs "stylesheet")
                             ! href (fs "./fotos.css") -- "http://www.w3schools.com/lib/w3.css")

--   liftIO $ forElems_ (fs "body") $ this ! style_ (fs "margin:10px;padding:10px")
   render $ rawHtml $ do
         h2 ! style_ (fs "color:black;margin-bottom:0px;font-weight:700;font-size:20px") $  "MARIA ALONSO"
         h4 ! style (fs "margin-top:0px") $ "Photography"
   render $ rawHtml $ do
         div ! id (fs "leftpane") ! clas (fs "leftpane")$ noHtml
         div ! id (fs "gallery") ! clas (fs "gallery") $ noHtml

   -- any event in the left pane or the navigation buttons refresh the gallery
   render $ at (fs "#leftpane") Insert $ leftPane  <++ br <|> choosePhoto  <|> return ()

   -- since the gallery refresing code below is downstream in the monad
   render $ at (fs "#gallery") Insert gallery

leftPane= do
   rawHtml $ do
      h3 ! style (fs "color:black") $ "Works"
      div ! id (fs "projects") $ noHtml
      br
      br
      h3 ! style (fs "color:black") $ "Bio"
      h3 ! style (fs "color:black") $ "Contact"

   at (fs "#projects")  Insert $ do
     Current (n,_,_) <- Widget $ getRData <|> return (Current (0,0,""))

     project <-  mconcat [wlink project (h4 project) <++ ptext n n'  txt | ((project,txt,_),n') <- zip projects [0..]]
     Widget $ setRData $ Current (fromJust $ findIndex (==project) $ map fst' projects,0,"")

   where
   ptext n n' txt=   when (n==n') $ p ! atr "align" (fs "justify") $  txt

fst' (x, _, _)= x
snd' (_, x, _)= x
trd  (_, _, x)= x

choosePhoto=  left <|> right

instance Monoid Int where
   mempty= 0
   mappend= (+)

style_= atr (fs "style")

gallery = do
  Current (n,m,classMove) <- Widget $ getRData <|> return (Current (0,0,""))
  let proj=(projects !!n)
  rawHtml $ do
        img ! clas (fs classMove)
                ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! m)
                ! style_ (fs "width:100%")
        img ! style (fs "visibility: hidden;width:0px;height:0px")
                ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! (m+1))


lengthImages pro= (projects !! pro) & trd & length

clas= atr (fs "class")
leftst= "w3-animate-left"
rightst= "w3-animate-right"

left = do

    wlink "left" (fs "<") ! clas (fs "w3-btn-floating")
                          ! id (fs "left")
    Current (n,m,_) <- Widget $ getRData <|> return (Current (0,0,""))
    Widget $ setRData . Current $
       if m == 0
         then  let n'= length projects -1
                 in if n == 0 then (n',lengthImages n' -1, leftst) else (n-1,lengthImages (n-1)-1, leftst)
         else (n,m-1, leftst)

right= do

    wlink "left" (fs ">") ! clas (fs "w3-btn-floating")
                          ! id (fs "right")
    Current (n,m,_) <- Widget $ getRData <|> return (Current (0,0,rightst))
    Widget $ setRData . Current $
       if m == lengthImages n - 1
         then if n == length projects - 1 then (0,0,rightst) else (n+1,0, rightst)
         else (n, m+1, rightst)


