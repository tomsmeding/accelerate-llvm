{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Marshal (

  module Data.Array.Accelerate.LLVM.Execute.Marshal

) where

import Data.Array.Accelerate.LLVM.Execute.Marshal

import Data.Array.Accelerate.LLVM.PTX.Target
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

import Data.Array.Accelerate.Array.Data

import qualified Foreign.CUDA.Driver                            as CUDA

import qualified Data.DList                                     as DL


instance Marshal PTX where
  type ArgR PTX = CUDA.FunParam

  marshalInt = CUDA.VArg
  marshalScalarData' t ad k
    | SingleArrayDict <- singleArrayDict t
    = Prim.withDevicePtr t ad $ \p -> do
        res <- k (DL.singleton (CUDA.VArg p))
        return (Nothing, res)

