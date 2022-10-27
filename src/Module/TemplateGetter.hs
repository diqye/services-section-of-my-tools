{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}
-- | mkSetAndOver ''Type
-- | _1 (1,2,3,4) -> 1 
module Module.TemplateGetter where
import Language.Haskell.TH.Syntax(VarBangType)
import Language.Haskell.TH
import Control.Monad(mapM)
  

mkSetAndOver :: Name -> Q [Dec]
mkSetAndOver name = do
  info <- reify name
  mkFromInfo info

mkFromInfo :: Info -> Q [Dec]
mkFromInfo (TyConI (DataD _ _ _ _ cons _)) = do
  decss <- mapM mkFromCons cons
  pure $ concat decss
mkFromInfo _ = do
  pure []

mkFromCons :: Con -> Q [Dec]
mkFromCons (RecC dataConName varBangs) = do
  decss <- mapM mkFromVarBangType varBangs
  pure $ concat decss
mkFromCons _ = pure []

-- | data Foo t = Foo {a::Int,b::String,c::t}
-- foo = Foo {a=1,b="b",c=8}
-- you will get 
-- s_a v rec@(Foo xx) = rec {a=v}
mkFromVarBangType :: VarBangType -> Q [Dec]
mkFromVarBangType var@(name,bang,typ) = do
  let setterName = mkName $ "set_" ++ nameBase name
  let overName = mkName $ "over_" ++ nameBase name
  v <- newName "v"
  fn <- newName "fn"
  foo <- newName "foo"

  pure [FunD setterName
    [Clause [VarP v,VarP foo]
      (NormalB (RecUpdE (VarE foo) [(name,VarE v)]))
      []]
    ,FunD overName
      [Clause [VarP fn,VarP foo]
        (NormalB (RecUpdE (VarE foo) [(name,(AppE (VarE fn) (AppE (VarE name) (VarE foo))))]))
        []]]

-- t1 (a,b) = a
-- set_t1 v = over_t1 (const v)
-- t2 (a,b) = b
-- s1 (a,b,c) = a
-- s2 (a,b,c) = b
-- s3 (a,b,c) = c
-- f1 (a,b,c,d) = a
-- f2 (a,b,c,d) = b
-- f3 (a,b,c,d) = c
-- f4 (a,b,c,d) = d
-- over_t1 fn (a,b) = (fn a ,b)
-- over_t2 fn (a,b) = (a ,fn b)
-- over_s1 fn (a,b,c) = (fn a,b,c)
-- over_s2 fn (a,b,c) = (a,fn b,c)
-- over_s3 fn (a,b,c) = (a,b,fn c)
-- over_f1 fn (a,b,c,d) = (fn a,b,c,d)
-- over_f2 fn (a,b,c,d) = (a,fn b,c,d)
-- over_f3 fn (a,b,c,d) = (a,b,fn c,d)
-- over_f4 fn (a,b,c,d) = (a,b,c,fn d)

class TupleGetter t a b c d | t->a,t->b,t->c,t->d where
  _1 :: t -> a
  _2 :: t -> b
  _3 :: t -> c
  _4 :: t -> d
  over_1 :: (a->a) -> t -> t
  over_2 :: (b->b) -> t -> t
  over_3 :: (c->c) -> t -> t
  over_4 :: (d->d) -> t -> t
  set_1 :: a -> t -> t
  set_2 :: b -> t -> t
  set_3 :: c -> t -> t
  set_4 :: d -> t -> t

instance TupleGetter (a,b,c,d) a b c d where
  _1 (a,b,c,d) = a
  _2 (a,b,c,d) = b
  _3 (a,b,c,d) = c
  _4 (a,b,c,d) = d
  over_1 fn (a,b,c,d) = (fn a,b,c,d)
  over_2 fn (a,b,c,d) = (a,fn b,c,d)
  over_3 fn (a,b,c,d) = (a,b,fn c,d)
  over_4 fn (a,b,c,d) = (a,b,c,fn d)
  set_1 v = over_1 (const v)
  set_2 v = over_2 (const v)
  set_3 v = over_3 (const v)
  set_4 v = over_4 (const v)

instance TupleGetter (a,b,c) a b c () where 
  _1 (a,b,c) = a
  _2 (a,b,c) = b
  _3 (a,b,c) = c
  _4 (a,b,c) = ()
  over_1 fn (a,b,c) = (fn a,b,c)
  over_2 fn (a,b,c) = (a,fn b,c)
  over_3 fn (a,b,c) = (a,b,fn c)
  over_4 fn (a,b,c) = (a,b,c)
  set_1 v = over_1 (const v)
  set_2 v = over_2 (const v)
  set_3 v = over_3 (const v)
  set_4 v = over_4 (const v)

instance TupleGetter (a,b) a b () () where 
  _1 (a,b) = a
  _2 (a,b) = b
  _3 (a,b) = ()
  _4 (a,b) = ()
  over_1 fn (a,b) = (fn a,b)
  over_2 fn (a,b) = (a,fn b)
  over_3 fn (a,b) = (a,b)
  over_4 fn (a,b) = (a,b)
  set_1 v = over_1 (const v)
  set_2 v = over_2 (const v)
  set_3 v = over_3 (const v)
  set_4 v = over_4 (const v)

  -- tuple5
instance TupleGetter (a,b,c,d,e) a b c d where
  _1 (a,b,c,d,e) = a
  _2 (a,b,c,d,e) = b
  _3 (a,b,c,d,e) = c
  _4 (a,b,c,d,e) = d
  over_1 fn (a,b,c,d,e) = (fn a,b,c,d,e)
  over_2 fn (a,b,c,d,e) = (a,fn b,c,d,e)
  over_3 fn (a,b,c,d,e) = (a,b,fn c,d,e)
  over_4 fn (a,b,c,d,e) = (a,b,c,fn d,e)
  set_1 v = over_1 (const v)
  set_2 v = over_2 (const v)
  set_3 v = over_3 (const v)
  set_4 v = over_4 (const v)
  -- tuple6
instance TupleGetter (a,b,c,d,e,f) a b c d  where
  _1 (a,b,c,d,e,f) = a
  _2 (a,b,c,d,e,f) = b
  _3 (a,b,c,d,e,f) = c
  _4 (a,b,c,d,e,f) = d
  over_1 fn (a,b,c,d,e,f) = (fn a,b,c,d,e,f)
  over_2 fn (a,b,c,d,e,f) = (a,fn b,c,d,e,f)
  over_3 fn (a,b,c,d,e,f) = (a,b,fn c,d,e,f)
  over_4 fn (a,b,c,d,e,f) = (a,b,c,fn d,e,f)
  set_1 v = over_1 (const v)
  set_2 v = over_2 (const v)
  set_3 v = over_3 (const v)
  set_4 v = over_4 (const v)