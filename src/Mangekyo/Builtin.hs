{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Mangekyo.Builtin where

import Control.Lens
import Data.Conduit

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Text.ICU (regex', find, group, unfold)
import System.Exit (ExitCode(..), exitWith)

import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Mangekyo.Interpreter
import Mangekyo.Lens
import Mangekyo.Type as Type
import Mangekyo.Builtin.Fmt (fmt)

builtins :: [(Text, Value)]
builtins =
    [ ("null", Null)
    , ("true", Bool True)
    , ("false", Bool False)

    , ("string", function1 string)
    , ("number", function1 number)
    , ("bool", function1 bool)

    , ("if", function3M if_)
    , ("pass", function1 pass_)
    , ("when", function2M when_)
    , ("unless", function2M unless_)

    , ("exit", function1M exit_)

    , (".", function2 dot)
    , ("id", function1 id)
    , ("const", function2 const)
    , ("for", function2M for_)
    , ("items", function1 items_)
    , ("fmt", function1M fmt)
    , ("split", function2 split_)
    , ("not", function1 not_)

    -- conduit functions
    , ("yield", function1M yield_)
    , ("await", function1M await_)
    , ("fold", function2M fold_)
    , ("map", function1M map_)
    , ("filter", function1M filter_)
    , ("exclude", function1M exclude_)
    , ("each", function1M each_)
    , ("concat", function1M concat_)
    , ("consume", function1M consume_)
    , ("concatMap", function1M concatMap_)
    , ("isolate", function1M isolate_)
    , ("chunksOf", function1M chunksOf_)
    , ("iterate", function2M iterate_)
    , ("mergeSource", function1M mergeSource_)
    , ("merge", function1M mergeSource_)
    , ("zipConduit", function2M zipConduit_)
    , ("zip", function2M zipConduit_)
    , ("sourceArray", function1M sourceArray_)
    , ("source", function1M sourceArray_)
    , ("leftover", function1M leftover_)
    , ("replicate", function2M replicate_)
    , ("peek", function1M peek_)

    -- lens
    , ("at", function1 at__)
    , ("view", function2M view_)
    , ("set", function2 set_)
    , ("over", function2 over_)
    , ("mapped", Type.Setter mapped_)
    , ("^.", function2M view_)
    , (".~", function2 set_)
    , ("%~", function2 over_)
    , (".=", function2M assign_)
    , ("%=", function2M modifying_)
    , ("+=", function2M modifyAdd_)

    -- operators
    , ("$", function2M apply_)
    , ("&", function2M reverseApply_)
    , ("+", function2 add_)
    , ("*", function2 mul_)
    , ("/", function2 div_)
    , ("==", function2 equal)
    , (">", function2 gt)
    , ("<", function2 lt)
    , (">=", function2 gte)
    , ("<=", function2 lte)
    , ("=~", function2 match_)
    , ("!~", function2 notMatch_)

    , ("negative", function1 negative_)

    , ("p", function1M $ \v -> liftIO (print v) >> return Null)
    ]

for_ :: Value -> Value -> Mangekyo Value
for_ (Array v) f = Array <$> V.mapM (funcCall f) v
for_ v f = for_ (array v) f

items_ :: Value -> Value
items_ (Object h) = Array $ V.fromList $ map (\(k, v) -> Tuple [String k, v]) $ H.toList h
items_ v = error $ "not a object: " ++ show v

-- TODO: split using regex
split_ :: Value -> Value -> Value
split_ (String sep) (String t) = Array $ V.fromList $ map String $ T.splitOn sep t
split_ sep s = split_ (string sep) (string s)

yield_ :: Value -> Mangekyo Value
yield_ v = yield v >> return Null

await_ :: Value -> Mangekyo Value
await_ _ = fromMaybe Null <$> await

fold_ :: Value -> Value -> Mangekyo Value
fold_ f i = go i
  where
    go acc = do
        mv <- await
        case mv of
            Nothing -> return acc
            Just v  -> do
                acc' <- funcCall f acc >>= \f' -> funcCall f' v
                go acc'

map_ :: Value -> Mangekyo Value
map_ f = do
    awaitForever $ \v -> do
        v' <- funcCall f v
        yield v'
    return Null

filter_ :: Value -> Mangekyo Value
filter_ f = do
    awaitForever $ \v -> do
        ret <- funcCall f v
        when (bool' ret) $ yield v
    return Null

exclude_ :: Value -> Mangekyo Value
exclude_ f = do
    awaitForever $ \v -> do
        ret <- funcCall f v
        when (not $ bool' ret) $ yield v
    return Null

each_ :: Value -> Mangekyo Value
each_ f = do
    awaitForever $ \v -> do
        _ <- funcCall f v
        return ()
    return Null

concat_ ::  Value -> Mangekyo Value
concat_ _ = do
    awaitForever concat_'
    return Null
  where
    concat_' (Array v) = V.mapM_ yield v
    concat_' v = concat_' $ array v

concatMap_ :: Value -> Mangekyo Value
concatMap_ f = (map_ f >> return ()) =$= concat_ Null

consume_ :: Value -> Mangekyo Value
consume_ _ = do
    v <- CL.consume
    return $ Array $ V.fromList v

isolate_ :: Value -> Mangekyo Value
isolate_ v = do
    CL.isolate (round $ number' v)
    return Null

chunksOf_ :: Value -> Mangekyo Value
chunksOf_ v = do
    CL.chunksOf (round $ number' v) =$= CL.map (Array . V.fromList)
    return Null

iterate_ :: Value -> Value -> Mangekyo Value
iterate_ f v = do
    v' <- funcCall f v
    yield v'
    iterate_ f v'

mergeSource_ :: Value -> Mangekyo Value
mergeSource_ f = do
    mergeSource (CL.sourceNull =$= funcCall f unit >> return ()) =$= CL.map (\(v1, v2) -> Tuple [v1, v2])
    return Null

zipConduit_ :: Value -> Value -> Mangekyo Value
zipConduit_ f1 f2 = do
    (r1, r2) <- getZipConduit $ (,) <$> ZipConduit (funcCall f1 unit)
                                    <*> ZipConduit (funcCall f2 unit)
    return $ Tuple [r1, r2]

sourceArray_ :: Value -> Mangekyo Value
sourceArray_ a@(Array v) = V.mapM_ yield v >> return a
sourceArray_ v = sourceArray_ $ array v

leftover_ :: Value -> Mangekyo Value
leftover_ v = leftover v >> return Null

replicate_ :: Value -> Value -> Mangekyo Value
replicate_ (Number s) v = CL.replicate (round s) v >> return Null
replicate_ n v = replicate_ (number n) v

peek_ :: Value -> Mangekyo Value
peek_ _ = do
    mv <- CL.peek
    return $ fromMaybe Null mv

if_ :: Value -> Value -> Value -> Mangekyo Value
if_ (Bool True) f _ = funcCall f unit
if_ (Bool False) _ f = funcCall f unit
if_ v f f' = if_ (bool v) f f'

pass_ :: Value -> Value
pass_ _ = Null

when_ :: Value -> Value -> Mangekyo Value
when_ b f = if_ b f (function1 pass_)

unless_ :: Value -> Value -> Mangekyo Value
unless_ b f = if_ b (function1 pass_) f

exit_ :: Value -> Mangekyo Value
exit_ (Tuple []) = exit_ (Number 0)
exit_ v =
    liftIO $ exitWith $
        case round (number' v) of
            0 -> ExitSuccess
            i -> ExitFailure i

dot :: Value -> Value -> Value
dot (Type.Lens l) (Type.Lens k) = Type.Lens (l . k)
dot (Type.Lens l) (Type.Setter k) = Type.Setter (l . k)
dot (Type.Setter l) (Type.Lens k) = Type.Setter (l . k)
dot (Type.Setter l) (Type.Setter k) = Type.Setter (l . k)
dot f g = Function $ \v -> do
    v' <- funcCall g v
    funcCall f v'

view_ :: Value -> Value -> Mangekyo Value
view_ v (Type.Lens l) = pure v ^. l
view_ _ o = error $ "not a lens: " ++ show o

at__ :: Value -> Value
at__ v = Type.Lens $ at_ v

assign_ :: Value -> Value -> Mangekyo Value
assign_ (Type.Lens l) v = assign_ (Type.Setter l) v
assign_ (Type.Setter s) v = do
    s' <- get & s .~ pure v
    put s'
    return v
assign_ o _ = error $ "not a lens: " ++ show o

set_ :: Value -> Value -> Value
set_ (Type.Lens l) v = set_ (Type.Setter l) v
set_ (Type.Setter s) v = function1M $ (s .~ return v) . pure
set_ v _ = error $ "not a lens: " ++ show v

over_ :: Value -> Value -> Value
over_ (Type.Lens l) f = over_ (Type.Setter l) f
over_ (Type.Setter s) f = function1M $ (s %~ (>>= funcCall f)) . pure
over_ v _ = error $ "not a lens: " ++ show v

modifying_ :: Value -> Value -> Mangekyo Value
modifying_ (Type.Lens l) f = do
    s <- get
    v <- pure s ^. l
    v' <- funcCall f v
    s' <- pure s & l .~ pure v'
    put s'
    return v'
modifying_ l@(Type.Setter _) f = do
    s <- get
    s' <- funcCall (over_ l f) s
    put s'
    -- TODO: can I get "v'" which is returned at Lens case?
    --return v'
    return Null

modifyAdd_ :: Value -> Value -> Mangekyo Value
modifyAdd_ l v = modifying_ l (function1 $ \w -> add_ w v)

apply_ :: Value -> Value -> Mangekyo Value
apply_ f v = funcCall f v

reverseApply_ :: Value -> Value -> Mangekyo Value
reverseApply_ v f = funcCall f v

add_ :: Value -> Value -> Value
add_ (Number n) (Number m) = Number (n + m)
add_ n@(Number _) v = add_ n (number v)
add_ (String s) (String t) = String (s <> t)
add_ s@(String _) v = add_ s (string v)
add_ (Object h) (Object i) = Object (i <> h) -- ("i" overwrite "h")
add_ o@(Object _) v = add_ o (object v)
add_ (Array v) (Array w) = Array (v <> w)
add_ a@(Array _) v = add_ a (array v)
add_ Null n@(Number _) = add_ (number Null) n
add_ Null s@(String _) = add_ (string Null) s
add_ Null o@(Object _) = add_ (object Null) o
add_ Null a@(Array _) = add_ (array Null) a
-- TODO: other types

equal :: Value -> Value -> Value
equal v v' = Bool (v == v')

gt :: Value -> Value -> Value
gt (Number n) (Number m) = Bool (n > m)
-- TODO: other types

lt :: Value -> Value -> Value
lt (Number n) (Number m) = Bool (n < m)
-- TODO: other types

gte :: Value -> Value -> Value
gte (Number n) (Number m) = Bool (n >= m)
-- TODO: other types

lte :: Value -> Value -> Value
lte (Number n) (Number m) = Bool (n <= m)
-- TODO: other types

mul_ :: Value -> Value -> Value
mul_ (Number n) (Number m) = Number (n * m)
mul_ n@(Number _) v = mul_ n (number v)
mul_ Null n@(Number _) = mul_ (number Null) n
mul_ (String s) (Number m)
    | Just i <- toBoundedInteger m = String (T.replicate i s)
--mul_ s@(String _) v = mul_ s (string v)
-- TODO: other types

div_ :: Value -> Value -> Value
div_ (Number n) (Number m) =

    let n' = toRealFloat n :: Double
        m' = toRealFloat m :: Double
    in Number $ fromFloatDigits $ n' / m'
div_ n@(Number _) v = div_ n (number v)
-- TODO: other types

negative_ :: Value -> Value
negative_ (Number s) = Number (-s)
negative_ v = negative_ (number v)

match_ :: Value -> Value -> Value
match_ (String t) (String pat) =
    case regex' [] pat of
        Left e  -> error $ show e
        Right r -> maybe Null (Array . V.fromList . map String . unfold group) $ find r t
match_ v w = match_ (string v) (string w)

notMatch_ :: Value -> Value -> Value
notMatch_ s pat = not_ $ match_ s pat

-- create `Function` from Value to Value function
function1 :: (Value -> Value) -> Value
function1 f = Function $ return . f

-- for monadic function
function1M :: (Value -> Mangekyo Value) -> Value
function1M f = Function f

function2 :: (Value -> Value -> Value) -> Value
function2 f = Function (\x -> return $ Function (\y -> return $ f x y))

function2M :: (Value -> Value -> Mangekyo Value) -> Value
function2M f = Function (\x -> return $ Function (\y -> f x y))

function3M :: (Value -> Value -> Value -> Mangekyo Value) -> Value
function3M f = Function (\x -> return $ Function (\y ->  return $ Function (\z -> f x y z)))
