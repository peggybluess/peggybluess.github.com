{-# LANGUAGE NoMonomorphismRestriction, CPP #-}
import Transient.Base
import Transient.Move

import GHCJS.HPlay.View as V hiding (map,head)
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

    keep $ initNode (empty :: Cloud ())  -- a web server for tests (files to serve must be in the folder "./static/out.jsexe")
#else

  runBody $ Widget $  do
   setRData $ Current (0,0,"")

   insertStyles

   panels
   -- any event in the left pane  or the initial photo refresh the gallery
   chooseProject  <|>  reinitpage <|> initialPhoto 

   -- since the gallery rendering code is downstream in the monad
   renderGallery 

initialPhoto=  render $  at (fs "#gallery") Insert $ do 
         let m= 0; n=0; proj= projects !! n

         rawHtml $ forElemId (fs "nav")  clear

         img    ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! m)
                ! style (fs "width:100%")
           `pass` OnClick
         return ()

insertStyles=
   liftIO $ addHeader $ link ! atr (fs "rel") (fs "stylesheet")
                             ! href (fs "fotos.css") 

-- | the skeleton of the app
panels= do
   render $ rawHtml $ do
         h2 ! id (fs "init") ! style (fs "color:black;margin-bottom:0px;font-weight:700;font-size:20px") $  "MARIA ALONSO"
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
         div ! id (fs "nav") $ noHtml



renderGallery= do
   clikableGallery <|>  leftRight
               -- click in the gallery would render the next photo recursively
   renderGallery
   where
   clikableGallery= do
     render $ at (fs "#gallery") Insert gallery
     norender forward

reinitpage= do
   render $ at (fs "#init") Insert $ wlink "init"  (fs "MARIA ALONSO")  ! style (fs "color:black")
   setRData $ Current (0,0,"") 
   initialPhoto


chooseProject= do


    project <- render $ at (fs "#projects") Insert  $ do
                             mconcat [ wlink project (h4 ! id (fs project) $ project) 
                                                            | (project,txt,_) <-  projects]

    Current (n,_,_) <-  getRData <|> return (Current (0,0  ,"") )

    let n'= fromJust $ findIndex (==project) $ map fst' projects
    setRData $ Current (n',0,"")

    -- render $ rawHtml $ do
    --     forElemId (fs "left")  $ this ! style (fs "visibility:hidden") 
    --     forElemId (fs "right") $ this ! style (fs "visibility:visible") 


    when (n /= n')  . render $ rawHtml $ do
       forElemId (fs $ fst' (projects !! n))   $ this ! clas (fs "other")
       forElemId (fs $ fst' (projects !! n'))  $ this ! clas (fs "highlighted")

    clicableText n' <|> leftRight
    return ()
    where
    clicableText n'=  do
       render $ at (fs "#gallery")  Insert $ 
                           (p ! atr "align" (fs "justify")
                             $ snd' (projects !! n'))  `pass`  OnClick
       return ()

fst' (x, _, _)= x 
snd' (_, x, _)= x
trd  (_, _, x)= x


instance Monoid Int where
   mempty= 0
   mappend= (+)

-- style= atr (fs "style")

-- | display the current image. it stop, and continue when the image is clicked (OnClick)
gallery = do
    Current (n,m,classMove) <- Widget $ getRData <|> return (Current (0,0,""))

    let proj=(projects !!n)

  -- preload next photo
--  rawHtml $ img ! style (fs "visibility: hidden;width:0px;height:0px")
--                ! src (fs $ "./"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! (m+1))

    img ! clas (fs classMove)
                    ! src (fs $ "../"++files++"/"++(proj & fst')++ "/"++ ( proj & trd) !! m)
                    ! style (fs "width:100%")
            `pass` OnClick
    return()


lengthImages pro= (projects !! pro) & trd & length

clas= atr (fs "class")
leftst= "w3-animate-left"
rightst= "w3-animate-right"


leftRight= staticNav $  render $ at (fs "#nav") Insert (left  <|> right)
   
left  = do
      Current (n,m,_) <-  Widget $ getRData <|> return (Current (0,0,""))
      if (m > 0) 
       then do
            wlink "left"  ( fs "Prev")   <++ toElem " | " -- ! id (fs "left") !  clas (fs "w3-btn-floating"
            backward
       else empty


backward = Widget $ do
      Current (n,m,_) <- getRData <|> return (Current (0,0,""))
      let m'= m-1
                  -- if m == 0
                  --      then  let n'= length projects -1
                  --            in if n == 0 then (n',lengthImages n' -1) else (n-1,lengthImages (n-1)-1)
                  --    else (n,m-1)


      setRData $ Current (n,m',leftst)
      -- when (n' /= n) $ changeText n n'
    --   render . rawHtml $ do
    --       when (m == lengthImages n ) $ forElemId (fs "right") $ this ! style (fs "visibility:visible") 
    --       when (m'==0)                $ forElemId (fs "left" ) $ this ! style (fs "visibility:hidden")


right = do
  Current (n,m,_) <-  Widget $ getRData <|> return (Current (0,0,""))
  if (m < lengthImages n -1 )
    then  do
      wlink "right" ( fs "Next")     --   ! clas (fs "w3-btn-floating") 
      forward
    else empty

forward =  Widget $ do
    Current (n,m,_) <- getRData <|> return (Current (0,0,rightst))
    let m'= m+1
    -- let (n',m')=  if m == lengthImages n - 1
    --                  then if n == length projects - 1 then (0,0) else (n+1,0)
    --                  else (n, m+1)

    setRData $ Current (n,m',rightst)
    -- when (n' /= n) $ changeText n n'
    -- render . rawHtml $ do
    --      when (m'== lengthImages n )  $ forElemId (fs "right") $ this ! style (fs "visibility:hidden") 
    --      when (m==0)                  $ forElemId (fs "left" ) $ this ! style (fs "visibility:visible")








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
