module NemoLib.Select where

select :: (a -> Bool) -> [a] -> [a]
select = filter
