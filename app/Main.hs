{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           ExQL

data UserProfile = UserProfile
                 { userName  :: String
                 , userScore :: Double
                 , pushCount :: Count
                 } deriving (Show)

instance Context UserProfile where
  data That UserProfile
    = UserScoreInc Double
    | UserScoreDec Double
    | UserRename String
    | UserPushNotify String
    deriving (Eq,Show)

  adapt usr@UserProfile{..} (UserScoreInc n)
    = usr {userScore = userScore + n}
  adapt usr@UserProfile{..} (UserScoreDec n)
    = usr {userScore = userScore - n}
  adapt usr@UserProfile{..} (UserRename r)
    = usr {userName  = r}
  adapt usr@UserProfile{..} (UserPushNotify _)
    = usr {pushCount = adapt pushCount (CountInc 1)}

isGoodUser :: This UserProfile
isGoodUser = Func1 (Const 0.6) (\c v -> userScore c > v)

userLogic :: Monad m => Logic UserProfile m ()
userLogic = do
  always
    [ Func0 $ \UserProfile{..} -> UserScoreInc (userScore + 0.1) ]

  branch isGoodUser
    [ Const (UserPushNotify "Thanks") ]
    [ Const (UserScoreDec 0.1) ]

newtype Count = Count Int
  deriving (Show,Eq,Ord)

instance Context Count where
  data That Count
    = CountInc Int
    | CountDec Int
    deriving (Eq,Show)

  adapt (Count c) (CountInc n) = Count $ c + n
  adapt (Count c) (CountDec n) = Count $ c - n

countLogic :: Monad m => Logic Count m ()
countLogic = always [ Const (CountInc 1) ]

main :: IO ()
main = do
  let user = UserProfile {userName = "john", userScore = 0.7, pushCount = Count 0}
  putStrLn $ show user
  mx <- runLogic user userLogic
  print mx
