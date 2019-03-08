{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseHeadersForNN where

import Data.Char (toLower)
import Data.Void (Void)
import Text.Megaparsec as M
import Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import ParseFunctionSig (TenType(..),CType(..),Parsable(..),Parameter(..))

{-
Spec:
  spec/THNN.h
  spec/THCUNN.h

Original parser:
  deps/pytorch/aten/src/ATen/common_with_cwrap.py
-}

type Parser = Parsec Void String


data Function  = Function
  { fname :: String
  , cname :: String
  , parameters :: [Parameter]
  } deriving (Eq, Show)

type HNN = Map String Function

sc :: Parser ()
sc = L.space space1 empty empty

lexm :: Parser a -> Parser a
lexm = L.lexeme sc


comment :: Parser ()
comment =
  try (newline >> return ()) <|>
  try (string "#" >> manyTill anySingle newline >> return ()) <|>
  try (string "//" >> manyTill anySingle newline >> return ()) <|>
  (string "/*" >> manyTill anySingle (string "*/") >> return ())

-- | parser of a function
-- >>> parseTest (comment >> string "1") "#define FOO 1\n1"
-- "1"
-- >>> parseTest (comments >> string "1") "#define FOO 1\n1"
-- "1"
-- >>> parseTest (comments >> string "1") "#define FOO 1\n#define BAR 1\n1"
-- "1"
-- >>> parseTest (comments >> string "1") "#define FOO 1\n\n#define BAR 1\n\n1"
-- "1"
-- >>> parseTest (comments >> string "1") "#define FOO 1\n//aaaa\n\n#define BAR 1\n\n1"
-- "1"
comments :: Parser ()
comments = comment >> optional comments >> pure ()

-- | parser of type-identifier
-- >>> parseTest typ "THCIndexTensor"
-- TenType IndexTensor
-- >>> parseTest typ "THCState"
-- StateType
-- >>> parseTest typ "THCState "
-- StateType
-- >>> parseTest typ "THCState*"
-- Ptr StateType
-- >>> parseTest typ "THCTensor"
-- TenType Tensor
-- >>> parseTest typ "THCTensor*"
-- Ptr (TenType Tensor)
-- >>> parseTest typ "THGenerator"
-- GeneratorType
-- >>> parseTest typ "THIndexTensor"
-- TenType IndexTensor
-- >>> parseTest typ "THNNState"
-- StateType
-- >>> parseTest typ "THTensor"
-- TenType Tensor
-- >>> parseTest typ "THTensor*"
-- Ptr (TenType Tensor)
-- >>> parseTest typ "accreal"
-- TenType Scalar
-- >>> parseTest typ "bool"
-- CType CBool
-- >>> parseTest typ "double"
-- CType CDouble
-- >>> parseTest typ "int"
-- CType CInt
-- >>> parseTest typ "int64_t"
-- CType CInt64
-- >>> parseTest typ "void"
-- CType CVoid
typ :: Parser Parsable
typ = try ptr <|> noPtr
  where
    noPtr =
      generator <|>
      state <|>
      tensor <|>
      ctype
    ptr = do
      t <- noPtr
      _ <- (lexm $ string "*")
      pure $ Ptr t
    generator =
      ((lexm $ string "THGenerator") >> (pure $ GeneratorType))
    state =
      ((lexm $ string "THCState") >> (pure $ StateType)) <|>
      ((lexm $ string "THNNState") >> (pure $ StateType))
    tensor =
      ((lexm $ string "accreal") >> (pure $ TenType Scalar)) <|>
      ((lexm $ string "THTensor") >> (pure $ TenType Tensor)) <|>
      ((lexm $ string "THCTensor") >> (pure $ TenType Tensor)) <|>
      ((lexm $ string "THIndexTensor") >> (pure $ TenType IndexTensor)) <|>
      ((lexm $ string "THCIndexTensor") >> (pure $ TenType IndexTensor))
    ctype =
      ((lexm $ string "bool") >> (pure $ CType CBool)) <|>
      ((lexm $ string "void") >> (pure $ CType CVoid)) <|>
      ((lexm $ string "double") >> (pure $ CType CDouble)) <|>
      try ((lexm $ string "int64_t") >> (pure $ CType CInt64)) <|>
      ((lexm $ string "int") >> (pure $ CType CInt))

identStart :: [Char]
identStart = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

identLetter :: [Char]
identLetter = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'] ++ [':', '<', '>']

rws :: [String]
rws = []

identifier :: Parser String
identifier = (lexm . try) (p >>= check)
 where
  p = (:) <$> (oneOf identStart) <*> many (oneOf identLetter)
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

arg :: Parser Parameter
arg = do
  _ <- optional $ lexm comment
  v <- param
  _ <- optional $ lexm comment
  pure v
 where
  param = do
    pt <- typ
    pn <- identifier
    pure $ Parameter pt pn Nothing

-- | parser of a function
-- >>> parseTest func "TH_API void THNN_(GatedLinear_updateOutput)(    THNNState *state,    THTensor *input,   \n THTensor *output, \n int dim);"
-- Function {fname = "GatedLinear", cname = "updateOutput", parameters = [Parameter {ptype = Ptr StateType, pname = "state", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "input", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "output", val = Nothing},Parameter {ptype = CType CInt, pname = "dim", val = Nothing}]}
-- >>> parseTest func "TH_API void THNN_(GatedLinear_updateOutput)(THNNState *state,   // library's state\n  THTensor *input,THTensor *output,int dim);"
-- Function {fname = "GatedLinear", cname = "updateOutput", parameters = [Parameter {ptype = Ptr StateType, pname = "state", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "input", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "output", val = Nothing},Parameter {ptype = CType CInt, pname = "dim", val = Nothing}]}
func :: Parser Function
func = do
  _ <- try (manyTill (anySingle <|> newline) $ (string "TH_API void THNN_(" <|> string "THC_API void THNN_("))
  v <- identifier
  case splitOn "_" v of
    (fname':cname':[]) -> do
      _ <- lexm $ string ")("
      args <- (sepBy arg (lexm (string ",")))
      _ <- lexm $ string ");"
      pure $ Function fname' cname' args
    _ ->  fail $ "function name: " ++ show v ++ " cannot be splited by '_\'"


-- | parser of a function
-- >>> parseTest functions "TH_API void THNN_(GatedLinear_updateOutput)(THNNState *state,THTensor *input,THTensor *output,int dim);"
-- [Function {fname = "GatedLinear", cname = "updateOutput", parameters = [Parameter {ptype = Ptr StateType, pname = "state", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "input", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "output", val = Nothing},Parameter {ptype = CType CInt, pname = "dim", val = Nothing}]}]
-- >>> parseTest functions "#define FOO 1\nTH_API void THNN_(GatedLinear_updateOutput)(THNNState *state,THTensor *input,THTensor *output,int dim);"
-- [Function {fname = "GatedLinear", cname = "updateOutput", parameters = [Parameter {ptype = Ptr StateType, pname = "state", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "input", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "output", val = Nothing},Parameter {ptype = CType CInt, pname = "dim", val = Nothing}]}]
-- >>> parseTest functions "TH_API void THNN_(GatedLinear_updateOutput)(THNNState *state,THTensor *input,THTensor *output,int dim);\n"
-- [Function {fname = "GatedLinear", cname = "updateOutput", parameters = [Parameter {ptype = Ptr StateType, pname = "state", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "input", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "output", val = Nothing},Parameter {ptype = CType CInt, pname = "dim", val = Nothing}]}]
-- >>> parseTest functions "#define FOO 1\n\n#define BAR 1\n\nTH_API void THNN_(GatedLinear_updateOutput)(THNNState *state,THTensor *input,THTensor *output,int dim);\n\n"
-- [Function {fname = "GatedLinear", cname = "updateOutput", parameters = [Parameter {ptype = Ptr StateType, pname = "state", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "input", val = Nothing},Parameter {ptype = Ptr (TenType Tensor), pname = "output", val = Nothing},Parameter {ptype = CType CInt, pname = "dim", val = Nothing}]}]
functions :: Parser [Function]
functions =  do
  x <- optional func
  case x of
    Just x' -> do
      xs <- optional functions
      case xs of
        Nothing -> pure [x']
        Just xs' -> pure (x':xs')
    Nothing -> pure []

thnnArgs :: Function -> [Parameter]
thnnArgs fn = thnnArgs' $ parameters fn

thnnArgs' :: [Parameter] -> [Parameter]
thnnArgs' params =
  flip map params $ \p -> p{pname = rename $ camelToSnake (pname p)}
  where
    camelToSnake [] = []
    camelToSnake (x:xs) =
      if x `elem` ['A'..'Z']
      then '_':toLower(x):camelToSnake xs
      else x:camelToSnake xs
    rename "input" = "self"
    rename "weights" = "weight"
    rename "train" = "training"
    rename "val" = "value"
    rename "lambda" = "lambd"
    rename "negval" = "negative_slope"
    rename x = x

outputArgs :: String -> [Parameter] -> [Parameter]
outputArgs cname' params =
  map (\p' -> p'{ptype = typeConv (ptype p')}) $
  flip filter params $ \p ->
    pname p == "output" && cname' == "updateOutput" ||
    pname p `elem` ["grad_input", "grad_weight", "grad_bias", "grad_grid"] ||
    pname p == "indices" && cname' == "updateOutput"
  where
    typeConv (Ptr (TenType Tensor)) = TenType Tensor
    typeConv a = a

-- | see remove_unused_args(args, thnn_args) of nn_parse.py
-- Returns the subset of args whose name appears in thnn_args
removeUnusedArgs :: [Parameter] -> [Parameter] -> [Parameter]
removeUnusedArgs args thnn_args =
  flip filter args $ \arg' -> pname arg' `elem` map pname thnn_args
