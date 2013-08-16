{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Control.Monad.Quantum.SQCT (applySQGate, applySQGateList) where

import Control.Monad.Quantum.Base.Class
import Data.Quantum.SQCT

applySQGate :: MonadQuantumBase w m => SQGate -> w -> m ()
applySQGate SQGateId _ = return ()
applySQGate SQGateX w = applyX w
applySQGate SQGateY w = applyY w
applySQGate SQGateZ w = applyZ (bit w)
applySQGate SQGateH w = applyH w
applySQGate (SQGateS inv) w = applyS (bit w) inv
applySQGate (SQGateT inv) w = applyT (bit w) inv

applySQGateList :: MonadQuantumBase w m => [SQGate] -> w -> m ()
applySQGateList gs w =
    mapM_ (flip applySQGate w) gs
