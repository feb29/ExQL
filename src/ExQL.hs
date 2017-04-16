{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module ExQL
    ( Var(..)
    , evalVar

    , Context(..)
    -- , This, That

    , Logic'(..)
    , Logic
    , branch
    , always
    , runLogic
    ) where

import           Control.Monad.Operational
-- import qualified Data.DList                as D

{-

Always [Mail(DailyRecommend)]

If (Wheather($Yesterday) != RAIN && Wheather($Today) == RAIN)
  [Push(Coupon, 100)]
  [Push(Coupon, 200)]

Match
  (LTVScore($Audience) <= 0.5) => [AudienceScore(+1)]
  (LTVScore($Audience) <= 1.0) => [AudienceScore(+2)]

-}

-- When, Match

-- r: result
-- c: context
data Var c r where
  -- constant
  Const :: r -> Var c r

  -- function and arity
  Func0 :: (c -> r) -> Var c r
  Func1 :: Var c r1 -> (c -> r1 -> r) -> Var c r
  Func2 :: Var c r1 -> Var c r2 -> (c -> r1 -> r2 -> r) -> Var c r

class Context c where
  -- If This
  type This c :: *
  type This c = Var c Bool

  -- Then That
  data That c :: *

  adapt :: c -> That c -> c

  {-# INLINE adapt' #-}
  adapt' :: That c -> c -> c
  adapt' = flip adapt

data Logic' c r where
  Bottom
    :: Context c
    => That c
    -> Logic' c ()

  Branch
    :: Context c
    => Var c Bool
    -> [Var c (That c)]
    -> [Var c (That c)]
    -> Logic' c ()

type Logic c = ProgramT (Logic' c)

branch
  :: (Context c, Monad m)
  => Var c Bool
  -> [Var c (That c)]
  -> [Var c (That c)]
  -> Logic c m ()

branch p t e
  = singleton $ Branch p t e

always
  :: (Context c, Monad m)
  => [Var c (That c)]
  -> Logic c m ()

always ts
  = branch (Const True) ts []

runLogic
  :: (Context c, Monad m)
  => c -> Logic c m r
  -> m (c, [That c])

runLogic c = loop (c, []) where
  loop
    :: (Context c, Monad m)
    => (c, [That c])
    -> Logic c m r
    -> m (c, [That c])

  loop (ctx, ds) l = viewT l >>= \pg ->
    case pg of
      (Return _) ->
        return (ctx, ds)

      (Bottom t :>>= _) ->
        return (adapt ctx t, ds ++ [t])

      (Branch p t e :>>= k) ->
        if (evalVar ctx p)
           then loop (nextK t) (k ())
           else loop (nextK e) (k ())

   where
    nextK   xs = (folded xs, ds ++ mapvars xs)
    mapvars xs = map (evalVar ctx) xs
    folded  xs = foldl adapt ctx (mapvars xs)

-- | evaluate Var
evalVar :: c -> Var c r -> r
evalVar _ (Const a) = a
evalVar ctx (Func0 f) = f ctx
evalVar ctx (Func1 v1 f) = f ctx (evalVar ctx v1)
evalVar ctx (Func2 v1 v2 f) = f ctx (evalVar ctx v1) (evalVar ctx v2)
