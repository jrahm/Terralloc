{-# LANGUAGE TemplateHaskell #-}

module TileShow where
import Language.Haskell.TH

makeShow t = do
  TyConI (DataD _ _ _ constructors _)  <-  reify t
  -- Make `show` clause for one constructor:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  let showClause (NormalC name fields) = do
        -- Name of constructor, i.e. "A". Will become string literal in generated code
        let constructorName = [(head $ nameBase name)]
        -- Generate function clause for one constructor
        clause [conP name []]                                 -- (A x1 x2)
               (normalB [| constructorName |]) []  -- "A "++show x1++" "++show x2
  -- Make body for function `show`:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  --   show (B x1)    = "B "++show x1
  --   show C         = "C"
  showbody <- mapM showClause constructors
  -- Generate template instance declaration and then replace
  --   type name (T1) and function body (\x -> "text") with our data
  d <- [d| instance Show String where
             show _x = "text"
       |]
  let    [InstanceD [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
  return [InstanceD [] (AppT showt (ConT t  )) [FunD showf showbody]]
