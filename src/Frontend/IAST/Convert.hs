module Frontend.IAST.Convert where

import qualified Frontend.IAST.Abs as IAST
import qualified Frontend.Zedex.Abs as ZX

import Data.Complex ( Complex(..) )

testZXPrg :: ZX.Program
testZXPrg = ZX.Progr [topl]
    where topl = ZX.ToplX (toplC) ids expr
          toplC = ZX.CComp (ZX.Scalar "1") (ZX.Scalar "0")
          absC = ZX.CComp (ZX.Scalar "0.5") (ZX.Scalar "0")
          expr = ZX.EApp (ZX.EXAbs absC ids ZX.EUnit) (ZX.EComp (ZX.EVar id1) (ZX.EVar id2))
          id1 = ZX.Id ((0,0), "id0")
          id2 = ZX.Id ((0,0), "id1")
          ids = [id1, id2]

convert :: ZX.Program -> IAST.Program
convert (ZX.Progr topls) = IAST.Progr $ map convertTopl topls

convertTopl :: ZX.Toplevel -> IAST.Toplevel
convertTopl (ZX.ToplF name ids e) = IAST.ToplF (idConvert name) (idsConvert ids) (convertExpr e)
convertTopl (ZX.ToplX c ids e)    = IAST.Topl (IAST.X) (complexConvert c) (idsConvert ids) (convertExpr e)
converTTopl (ZX.ToplZ c ids e)    = IAST.Topl (IAST.Z) (complexConvert c) (idsConvert ids) (convertExpr e)

convertExpr :: ZX.Expr -> IAST.Expr
convertExpr (ZX.EVar var) = IAST.Var (idConvert var)
convertExpr (ZX.ETup e es) = IAST.Tup (convertExpr e) (map convertExpr es)
convertExpr (ZX.EUnit) = IAST.Unit
convertExpr (ZX.EApp e1 e2) = IAST.App (convertExpr e1) (convertExpr e2)
convertExpr (ZX.EComp e1 e2) = IAST.Comp (convertExpr e1) (convertExpr e2)
convertExpr (ZX.EHad e) = IAST.Had (convertExpr e)
convertExpr (ZX.EAbs ids e) = IAST.Abs IAST.None 0 (idsConvert ids) (convertExpr e)
convertExpr (ZX.EXAbs c ids e) = IAST.Abs IAST.X (complexConvert c) (idsConvert ids) (convertExpr e)
convertExpr (ZX.EZAbs c ids e) = IAST.Abs IAST.Z (complexConvert c) (idsConvert ids) (convertExpr e)

complexConvert :: ZX.Complex -> Complex Double
complexConvert (ZX.CComp (ZX.Scalar s1) (ZX.Scalar s2)) = (read s1) :+ (read s2)
complexConvert (ZX.CComn (ZX.Scalar s1) (ZX.Scalar s2)) = (read s1) :+ (negate (read s2))
complexConvert ZX.CPi = pi :+ 0
complexConvert ZX.CE = exp 1 :+ 0
complexConvert (ZX.CExp c1 c2) = (complexConvert c1)**(complexConvert c2)
complexConvert (ZX.CDiv c1 c2) = (complexConvert c1)/(complexConvert c2)
complexConvert (ZX.CMul c1 c2) = (complexConvert c1)*(complexConvert c2)


idConvert :: ZX.Id -> IAST.Id
idConvert (ZX.Id ((x, y), name)) = IAST.Id (x,y) name

idsConvert :: [ZX.Id] -> [IAST.Id]
idsConvert ids = map idConvert ids