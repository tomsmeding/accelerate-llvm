{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.State
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.State
  where

-- accelerate
import Data.Array.Accelerate.LLVM.Target.ClangInfo

-- llvm-pretty
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.PP as LP

-- standard library
import Control.Concurrent.MVar
import Control.Monad.Catch                              ( MonadCatch, MonadThrow, MonadMask )
import Control.Monad.Reader                             ( ReaderT(..), MonadReader, runReaderT )
import Control.Monad.State                              ( StateT(..), MonadState, evalStateT )
import Control.Monad.Trans                              ( MonadIO )
import Data.Maybe                                       ( fromJust )
import Prelude


-- Execution state
-- ===============

-- | The LLVM monad, for executing array computations. This consists of a stack
-- for the LLVM execution context as well as the per-execution target specific
-- state 'target'.
--
newtype LLVM target a = LLVM { runLLVM :: ReaderT LP.LLVMVer (StateT target IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LP.LLVMVer, MonadState target, MonadThrow, MonadCatch, MonadMask)

-- | Extract the execution state: 'gets llvmTarget'
--
llvmTarget :: t -> t
llvmTarget = id

-- | Evaluate the given target with an LLVM context
--
evalLLVM :: t -> LLVM t a -> IO a
evalLLVM target acc =
  case llvmverFromTuple hostLLVMVersion of
    Just version -> evalStateT (runReaderT (runLLVM acc) version) target
    Nothing -> fail "accelerate-llvm: Could not determine LLVM version from Clang output"

-- | Because 'LLVM' is a state monad, it cannot support running multiple
-- computations in parallel (it would be unclear how to merge the resulting
-- states of the parallel computations). Therefore, attempting to run the
-- @forall a. LLVM t a -> IO a@ handler multiple times in parallel will not
-- work as you like: it will __take a lock__ so that the invocations run
-- sequentially. Furthermore, running the handler after the @IO b@ computation
-- has returned will throw an asynchronous exception.
unliftIOLLVM :: ((forall a. LLVM t a -> IO a) -> IO b) -> LLVM t b
unliftIOLLVM f =
  -- If the representation of the 'LLVM' monad changes, this function will have
  -- to be revised anyway. Hence, using the monad constructors directly is
  -- fine.
  LLVM $
    ReaderT $ \llvmver ->
      StateT $ \instate -> do
        var <- newMVar (Just instate)
        res <- f (unlift llvmver var)
        outstate <- takeMVar var
        putMVar var Nothing  -- mark the handler as dead
        return (res, fromJust outstate)
  where
    unlift :: LP.LLVMVer -> MVar (Maybe target) -> LLVM target a -> IO a
    unlift llvmver var (LLVM (ReaderT g)) = do
      let StateT h = g llvmver
      modifyMVar var $ \mst ->
        case mst of
          Just st -> do
            (x, st') <- h st
            return (Just st', x)
          Nothing -> error "unliftIOLLVM: Handler called after computation has completed"


-- -- | Make sure the GC knows that we want to keep this thing alive forever.
-- --
-- -- We may want to introduce some way to actually shut this down if, for example,
-- -- the object has not been accessed in a while (whatever that means).
-- --
-- -- Broken in ghci-7.6.1 Mac OS X due to bug #7299.
-- --
-- keepAlive :: a -> IO a
-- keepAlive x = forkIO (caffeine x) >> return x
--   where
--     caffeine hit = do threadDelay (5 * 1000 * 1000) -- microseconds = 5 seconds
--                       caffeine hit

