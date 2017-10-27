module Type where
import Data.List(nub, intersect, union)

type Index  = Int
type Id     = String
data TI a   = TI (Index -> (a, Index))
type Subst  = [(Id, SimpleType)]
data Assump = Id :>: SimpleType deriving (Eq, Show)

data SimpleType  =  TVar Id
                  | TArr  SimpleType SimpleType
                  deriving Eq

instance Show SimpleType where
   show (TVar i) = i
   show (TArr (TVar i) t) = i++" -> "++show t   
   show (TArr t t') = "("++show t++")"++"->"++show t'          
--------------------------
instance Functor TI where
   fmap f (TI m) = TI (\e -> let (a, e') = m e in (f a, e'))

instance Applicative TI where
   pure a = TI (\e -> (a, e))
   TI fs <*> TI vs = TI (\e -> let (f, e') = fs e; (a, e'') = vs e' in (f a, e''))              

instance Monad TI where 
   return x = TI (\e -> (x, e))
   TI m >>= f  = TI (\e -> let (a, e') = m e; TI fa = f a in fa e')

freshVar :: TI SimpleType
freshVar = TI (\e -> let v = "t"++show e in (TVar v, e+1))

runTI (TI m) = let (t, _) = m 0 in t 
----------------------------
t --> t' = TArr t t' 

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

dom = map (\(i:>:_)->i)

as /+/ as' = as' ++ filter compl as
   where
     is = dom as'
     compl (i:>:_) = notElem i is
---------------------------- 
class Subs t where
  apply :: Subst -> t -> t
  tv    :: t -> [Id]

instance Subs SimpleType where
  apply s (TVar u)  =   
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply s (TArr l r) =  (TArr (apply s l) (apply s r))


  tv (TVar u)  = [u]
  tv (TArr l r) = tv l `union` tv r


instance Subs a => Subs [a] where
  apply s     = map (apply s)
  tv          = nub . concat . map tv
  
instance Subs Assump where
  apply s (i:>:t) = i:>:apply s t
  tv (i:>:t) = tv t
  
------------------------------------
varBind :: Id -> SimpleType -> Maybe Subst
varBind u t | t == TVar u   = Just []
            | u `elem` tv t = Nothing
            | otherwise     = Just [(u, t)]
      
mgu (TArr l r,  TArr l' r') = do s1 <- mgu (l,l')
                                 s2 <- mgu ((apply s1 r) ,  (apply s1 r'))
                                 return (s2 @@ s1)
mgu (t,        TVar u   )   =  varBind u t
mgu (TVar u,   t        )   =  varBind u t

unify t t' =  case mgu (t,t') of
    Nothing -> error ("\ntrying to unify:\n" ++ (show t) ++ "\nand\n" ++
                      (show t')++"\n")
    Just s  -> s
 
