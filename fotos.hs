{-# LANGUAGE NoMonomorphismRestriction, CPP #-}
import Transient.Base
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
type OnDisplay= (Project,Photo, Style)
data Current= Current OnDisplay



files = "files"


filterdir= return . filter ((/= '.') . head)

fs= fromString
main=  do
  -- when compiled with ghc, will generate the "content" by reading the content of "files"
#ifndef ghcjs_HOST_OS
--  -- Just generates the "content" file
--  prs <- liftIO $ getDirectoryContents files >>= filterdir
--  projects <- liftIO $ mapM (\f -> (getDirectoryContents $ files ++ "/" ++ f) >>= filterdir)  prs
--  liftIO $ writeFile "content" $ "\t"++ (show $ zip3 (reverse prs) "" projects)

    keep $ initNode empty  -- a web server for tests (files to serve must be in the folder "./static/out.jsexe")
#else

  runBody $ Widget $  do
   setRData $ Current (0,0,"")

   insertStyles

   panels

   -- any event in the left pane or the navigation buttons refresh the gallery
   chooseProject   <|> choosePhoto   <|> return ()

   -- since the gallery refresing code below is downstream in the monad

   renderGallery


insertStyles=
   liftIO $ addHeader $ link ! atr (fs "rel") (fs "stylesheet")
                             ! href (fs "fotos.css") -- "http://www.w3schools.com/lib/w3.css")

-- | the skeleton of the app
panels= do
   render $ rawHtml $ do
         h2 ! style_ (fs "color:black;margin-bottom:0px;font-weight:700;font-size:20px") $  "MARIA ALONSO"
         h4 ! style (fs "margin-top:0px") $ "Photography"

         div ! id (fs "leftpane") ! clas (fs "leftpane")$ do
               h3 ! style (fs "color:black") $ "Works"
               div ! id (fs "projects") $ noHtml
               br
               br
               h3 ! style (fs "color:black") $ "Bio"
               h3 ! style (fs "color:black") $ "Contact"
               br
         div ! id (fs "gallery") ! clas (fs "gallery") $ noHtml

renderGallery= do

   render $ at (fs "#gallery") Insert gallery
   forward            -- click in the gallery render the next photo recursively
   renderGallery





chooseProject= do


    project <- render $ at (fs "#projects") Insert  $ do
                             mconcat [ wlink project (h4 project) <++ ptext  n'  txt
                                                            | ((project,txt,_),n') <- zip projects [0..]]

    Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )

    let n'= fromJust $ findIndex (==project) $ map fst' projects
    when (n /= n') $ changeText n n'
    setRData $ Current (n',0,"")

   where

   ptext n txt=  div ! id (fs $ fst' (projects !! n)) $  if (n== 0) then p ! atr "align" (fs "justify") $  txt else noHtml

changeText n n' = render $  rawHtml $ do
    forElemId (fs $ fst' (projects !! n))  $ clear
    forElemId (fs $ fst' (projects !! n')) $ do clear ; p ! atr "align" (fs "justify") $  snd' (projects !! n')


fst' (x, _, _)= x
snd' (_, x, _)= x
trd  (_, _, x)= x

choosePhoto=  left <|> right

instance Monoid Int where
   mempty= 0
   mappend= (+)

style_= atr (fs "style")

-- | display the current image. it stop and continue when the image is clicked (OnClick)
gallery = do
  Current (n,m,classMove) <- Widget $ getRData <|> return (Current (0,0,""))

  let proj=(projects !!n)

  -- preload next photo
--  rawHtml $ img ! style (fs "visibility: hidden;width:0px;height:0px")
--                ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! (m+1))

  img ! clas (fs classMove)
                ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! m)
                ! style_ (fs "width:100%")
           `pass` OnClick


lengthImages pro= (projects !! pro) & trd & length

clas= atr (fs "class")
leftst= "w3-animate-left"
rightst= "w3-animate-right"

left = do
    render $ wlink "left" (fs "<") ! clas (fs "w3-btn-floating")  ! id (fs "left")
    backward


backward = do
    Current (n,m,_) <-  getRData <|> return (Current (0,0,""))
    let (n',m')= if m == 0
                     then  let n'= length projects -1
                             in if n == 0 then (n',lengthImages n' -1) else (n-1,lengthImages (n-1)-1)
                     else (n,m-1)


    setRData $ Current (n',m',leftst)
    when (n' /= n) $ changeText n n'


right= do
    render $ wlink "left" (fs ">") ! clas (fs "w3-btn-floating")  ! id (fs "right")
    forward

forward =  do
    Current (n,m,_) <- getRData <|> return (Current (0,0,rightst))
    let (n',m')=  if m == lengthImages n - 1
                     then if n == length projects - 1 then (0,0) else (n+1,0)
                     else (n, m+1)

    setRData $ Current (n',m',rightst)
    when (n' /= n) $ changeText n n'

-- STRefs for the Transient monad

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


#endif
