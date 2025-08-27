{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Execute.Marshal
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Execute.Marshal
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma, Idx'(..) )
import Data.Array.Accelerate.LLVM.State                         ( LLVM )
import Data.Array.Accelerate.LLVM.Execute.Environment
import Data.Array.Accelerate.LLVM.Execute.Async

import Control.Monad.Cont
import Data.DList                                               ( DList )
import qualified Data.DList                                     as DL
import Data.Functor.Compose
import qualified Data.IntMap                                    as IM


-- Marshalling arguments
-- ---------------------
class Async arch => Marshal arch where
  -- | A type family that is used to specify a concrete kernel argument and
  -- stream/context type for a given backend target.
  --
  type ArgR arch

  -- | Used to pass shapes as arguments to kernels.
  marshalInt :: Int -> ArgR arch

  -- | Pass arrays to kernels
  marshalScalarData' :: SingleType e -> ScalarArrayData e -> (DList (ArgR arch) -> LLVM arch r) -> LLVM arch r

-- | This is an 'Applicative', not a 'Monad'. If you need 'Monad'-like
-- functionality, take apart the 'Compose' and explicitly stage-separate the
-- 'Par' preprocessing and 'LLVM' code inside the keepalive section.
type ArgMarshaller arch r = Compose (Par arch) (ContT r (LLVM arch)) (DList (ArgR arch))

wrapArgMarshaller :: Marshal arch => ArgMarshaller arch r -> ([ArgR arch] -> LLVM arch r) -> Par arch r
wrapArgMarshaller m k = do
  f <- runContT <$> getCompose m
  liftPar (f (k . DL.toList))

-- | Convert function arguments into stream a form suitable for function calls
-- The functions ending in a prime return a DList and separate the Par actions
-- to run before the "critical section" that keeps the arrays alive, from the
-- actions in 'LLVM' that run in said keepalive section. The other functions
-- take a normal callback (albeit in 'LLVM') that takes a normal list.
--
marshalArrays :: forall arch arrs r. Marshal arch => ArraysR arrs -> arrs -> ([ArgR arch] -> LLVM arch r) -> Par arch r
marshalArrays repr arrs = wrapArgMarshaller (marshalArrays' repr arrs)

marshalArrays' :: forall arch arrs r. Marshal arch => ArraysR arrs -> arrs -> ArgMarshaller arch r
marshalArrays' = marshalTupR' marshalArray'

marshalArray' :: forall arch a r. Marshal arch => ArrayR a -> a -> ArgMarshaller arch r
marshalArray' (ArrayR shr tp) (Array sh a) =
  let arg2 = marshalShape' @arch shr sh
  in (`DL.append` arg2) <$> marshalArrayData' tp a

marshalArrayData' :: forall arch t r. Marshal arch => TypeR t -> ArrayData t -> ArgMarshaller arch r
marshalArrayData' TupRunit ()               = pure DL.empty
marshalArrayData' (TupRpair t1 t2) (a1, a2) = DL.append <$> marshalArrayData' t1 a1 <*> marshalArrayData' t2 a2
marshalArrayData' (TupRsingle t) ad
  | ScalarArrayDict _ s <- scalarArrayDict t
  = Compose (return (ContT (marshalScalarData' @arch s ad)))

marshalEnv :: forall arch aenv r. Marshal arch => Gamma aenv -> ValR arch aenv -> ([ArgR arch] -> LLVM arch r) -> Par arch r
marshalEnv g a = wrapArgMarshaller (marshalEnv' g a)

marshalEnv' :: forall arch aenv r. Marshal arch => Gamma aenv -> ValR arch aenv -> ArgMarshaller arch r
marshalEnv' gamma aenv
    = fmap DL.concat
    $ traverse (\(_, Idx' repr idx) -> Compose $ do  -- get the future as Par precomputation
                  fut <- get (prj idx aenv)
                  getCompose (marshalArray' @arch repr fut))
               (IM.elems gamma)

marshalShape :: forall arch sh. Marshal arch => ShapeR sh -> sh -> [ArgR arch]
marshalShape shr sh = DL.toList $ marshalShape' @arch shr sh

marshalShape' :: forall arch sh. Marshal arch => ShapeR sh -> sh -> DList (ArgR arch)
marshalShape' ShapeRz () = DL.empty
marshalShape' (ShapeRsnoc shr) (sh, n) = marshalShape' @arch shr sh `DL.snoc` marshalInt @arch n

type ParamsR arch = TupR (ParamR arch)

data ParamR arch a where
  ParamRarray  :: ArrayR (Array sh e) -> ParamR arch (Array sh e)
  ParamRmaybe  :: ParamR arch a       -> ParamR arch (Maybe a)
  ParamRfuture :: ParamR arch a       -> ParamR arch (FutureR arch a)
  ParamRenv    :: Gamma aenv          -> ParamR arch (ValR arch aenv)
  ParamRint    ::                        ParamR arch Int
  ParamRshape  :: ShapeR sh           -> ParamR arch sh
  ParamRargs   ::                        ParamR arch (DList (ArgR arch))

marshalParam' :: forall arch a r. Marshal arch => ParamR arch a -> a -> ArgMarshaller arch r
marshalParam' (ParamRarray repr)  a        = marshalArray' repr a
marshalParam' (ParamRmaybe _   )  Nothing  = pure DL.empty
marshalParam' (ParamRmaybe repr)  (Just a) = marshalParam' repr a
marshalParam' (ParamRfuture repr) future   = -- get the future as Par precomputation
                                             Compose $ getCompose . marshalParam' repr =<< get future
marshalParam' (ParamRenv gamma)   aenv     = marshalEnv' gamma aenv
marshalParam'  ParamRint          x        = pure $ DL.singleton $ marshalInt @arch x
marshalParam' (ParamRshape shr)   sh       = pure $ marshalShape' @arch shr sh
marshalParam'  ParamRargs         args     = pure args

marshalParamsStaged :: forall arch a r. Marshal arch => ParamsR arch a -> a -> Par arch ((DList (ArgR arch) -> LLVM arch r) -> LLVM arch r)
marshalParamsStaged params args = runContT <$> getCompose (marshalParams' params args)

marshalParams :: forall arch a r. Marshal arch => ParamsR arch a -> a -> ([ArgR arch] -> LLVM arch r) -> Par arch r
marshalParams params args = wrapArgMarshaller (marshalParams' params args)

marshalParams' :: forall arch a r. Marshal arch => ParamsR arch a -> a -> ArgMarshaller arch r
marshalParams' = marshalTupR' @arch (marshalParam' @arch)

{-# INLINE marshalTupR' #-}
marshalTupR' :: forall arch s a r. Marshal arch
             => (forall b. s b -> b -> ArgMarshaller arch r) -> TupR s a -> a -> ArgMarshaller arch r
marshalTupR' _ TupRunit         ()       = pure DL.empty
marshalTupR' f (TupRsingle t)   x        = f t x
marshalTupR' f (TupRpair t1 t2) (x1, x2) = DL.append <$> marshalTupR' @arch f t1 x1 <*> marshalTupR' @arch f t2 x2

