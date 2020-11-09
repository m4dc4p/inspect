{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies,  MultiParamTypeClasses, FlexibleInstances #-}
module User (
  User (..)
  , Admin (Admin)
  , Ordinary (Ordinary)
  , NewUser
  , ReadUser
  , UpdateUser
  , newUser
  , readUser
  , updateUser
) where
  
data User id email color = User id email color
type NewUser color = User () (Maybe String) color
type ReadUser color = User Int (Maybe String) color
type UpdateUser color = User Int (Maybe (Maybe String)) (Maybe color)

data Admin = Admin
data Ordinary = Ordinary
data Color

class Secret rle field result | rle field -> result 
instance Secret Admin Color String 
instance Secret Ordinary a () 

readUser :: (Secret rle Color color) => rle -> Int -> IO (Maybe (ReadUser color))
readUser = undefined

updateUser :: (Secret rle Color color) => rle -> UpdateUser color -> IO (Maybe (ReadUser color))
updateUser = undefined

newUser :: (Secret rle Color color) => rle -> color -> Maybe String -> IO (NewUser color)
newUser = undefined
