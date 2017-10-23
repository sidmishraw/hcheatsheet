--
-- h3.hs
-- @author Sidharth Mishra
-- @description Testing out Functors, Applicative Functors and Monads
-- @copyright 2017 Sidharth Mishra
-- @created Sun Oct 22 2017 17:16:35 GMT-0700 (PDT)
-- @last-modified Sun Oct 22 2017 17:16:35 GMT-0700 (PDT)
--

x -: f = f x

-- myadd x y = y + x
-- mysub x y = y - x
-- mymul x y = y * x
-- mydiv d = (\n -> case d of
--   0 -> error "div by 0"
--   d -> div n d)

-- Rewritten using Maybe Monad
myadd x y = Just $ y + x
mysub x y = Just $ y - x
mymul x y = Just $ y * x
mydiv d n = case d of
  0 -> Nothing
  d -> Just $ div n d

-- a foo function for doing div, mult using `do`
foo x = do
  y <- mydiv 60 x
  z <- myadd 3 9
  mymul y z