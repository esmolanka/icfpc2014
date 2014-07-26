
module Utils.Utils where

(<?>) :: Maybe b -> a -> Either a b
(Just v) <?> _ = Right v
Nothing <?> msg = Left msg
