{-# LANGUAGE MultiWayIf #-}

module AronUnicodeOp where

import           Control.Concurrent (threadDelay)
import           Control.Monad                        (unless, when)
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Data.Set                             (Set, delete, fromList)
                 
import qualified                            Data.Set         as S
import qualified                            Data.List        as L
import Graphics.Rendering.OpenGL            as               GL
import Graphics.Rendering.OpenGL.GLU.Matrix as               GM
import qualified                            Graphics.UI.GLFW as FW
import qualified                            Graphics.UI.GLUT as GLUT

import           System.Exit
import           System.IO
import           System.Random
import           GHC.Real
import           Data.Complex

import Control.Lens
    ( Field1(_1), Field2(_2), Field3(_3), Field4(_4), (<&>), (^.) )
import qualified Data.Vector as VU
import           AronModule
import qualified Text.Printf as PR
import Data.Typeable (typeOf)

import AronModule
import AronGraphic

                 

(∘) :: (Num a) => [[a]] -> [[a]] -> [[a]]
(∘) = multiMat
  
