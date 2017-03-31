{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Mangekyo.Interpreter where

import Control.Lens
import Control.Monad (foldM, forM, mapM)
import Data.Conduit

import Data.Monoid ((<>))

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T

import Mangekyo.AST as AST
import Mangekyo.Lens
import Mangekyo.Type as Type


interpret :: Program -> Mangekyo Value
interpret (Program es) = foldM (const expr) Null es

expr :: Expression -> Mangekyo Value
expr (AST.Function p body) = return $ Type.Function func
  where
    func :: Function
    func v = do
        case match p v of
            Nothing -> error "argument mismatch"
            Just ms -> do
                -- TODO: local namespace (?)
                modify $ \(Type.Object h) -> Type.Object (H.fromList ms <> h)
                case body of
                    [] -> return Null
                    _  -> foldM (const expr) Null body
-- TODO: use location
expr (FuncCall _ f v) =
    let f' = expr f
        v' = expr v
    in do
        f'' <- f'
        v'' <- v'
        funcCall f'' v''
expr (Variable name) = get ^. at_ (Type.String name)
expr (AST.String t) = return $ Type.String t
expr (AST.Number s) = return $ Type.Number s
expr (AST.Tuple es) = Type.Tuple <$> mapM expr es
expr (AST.Array es) = Type.Array . V.fromList <$> mapM expr es
expr (AST.Object kvs) = do
    kvs' <- flip mapM kvs $ \(k, v) -> do
        v' <- expr v
        return (k, v')
    return $ Type.Object $ H.fromList kvs'
expr (AST.Lens e) = do
    e' <- expr e
    return $ Type.Lens $ at_ e'
expr (AST.Pipe es) = pipe =<< mapM expr es

match :: Pattern -> Value -> Maybe [(T.Text, Value)]
match (PVariable v Nothing) o = Just [(v, o)]
match (PVariable v (Just p)) o = do
    vos <- match p o
    return $ (v, o) : vos
match (PString p) (Type.String t)
    | p == t = Just []
    | otherwise = Nothing
match (PNumber p) (Type.Number n)
    | p == n = Just []
    | otherwise = Nothing
match (PTuple ps) (Type.Tuple os) = matchList ps os
match (PArray ps) (Type.Array os) = matchList ps $ V.toList os
match (PObject ps) (Type.Object oh) = matchHash ps oh
match _ _ = Nothing

matchList :: [Pattern] -> [Value] -> Maybe [(T.Text, Value)]
matchList (p:ps) (o:os) = do
    m <- match p o
    ms <- matchList ps os
    return (m ++ ms)
matchList [] [] = Just []
matchList _ _ = Nothing

matchHash :: [(T.Text, Pattern)] -> H.HashMap T.Text Value -> Maybe [(T.Text, Value)]
matchHash ps oh = do
    mss <- forM ps $ \(t, p) ->
        case H.lookup t oh of
            Just o  -> match p o
            Nothing -> Nothing
    return $ concat mss

funcCall :: Value -> Value -> Mangekyo Value
funcCall (Type.Function f) v = f v
funcCall o _ = error $ "not a collable: " ++ typeName o

pipe :: [Value] -> Mangekyo Value
pipe (e:es) = foldl (\m f -> (m >> return ()) =$= (funcCall f unit)) (funcCall e unit) es
