module Frontend.IAST.Convert where

import qualified Frontend.IAST.Abs as IAST
import qualified Frontend.Zedex.Abs as ZX

import Data.Complex ( Complex(..) )

convert :: ZX.Program -> IAST.Program
convert (ZX.Progr topls) = IAST.Progr $ map convertTopl topls

convertTopl :: ZX.Toplevel -> IAST.Toplevel
convertTopl (ZX.ToplF name ids e) = IAST.ToplF (convertId name) (convertIds ids) (convertExpr e)
convertTopl (ZX.ToplX c ids e)    = IAST.Topl IAST.X (complexConvert c) (convertIds ids) (convertExpr e)
convertTopl (ZX.ToplZ c ids e)    = IAST.Topl IAST.Z (complexConvert c) (convertIds ids) (convertExpr e)

convertExpr :: ZX.Expr -> IAST.Expr
convertExpr (ZX.EVar var) = IAST.Var (convertId var)
convertExpr (ZX.ETup e es) = IAST.Tup (convertExpr e) (map convertExpr es)
convertExpr ZX.EUnit = IAST.Unit
convertExpr (ZX.EApp e1 e2) = IAST.App (convertExpr e1) (convertExpr e2)
convertExpr (ZX.EComp e1 e2) = IAST.Comp (convertExpr e1) (convertExpr e2)
convertExpr (ZX.EHad e) = IAST.Had (convertExpr e)
convertExpr (ZX.EAbs ids e) = IAST.Abs IAST.None 0 (convertIds ids) (convertExpr e)
convertExpr (ZX.EXAbs c ids e) = IAST.Abs IAST.X (complexConvert c) (convertIds ids) (convertExpr e)
convertExpr (ZX.EZAbs c ids e) = IAST.Abs IAST.Z (complexConvert c) (convertIds ids) (convertExpr e)

complexConvert :: ZX.Complex -> Complex Double
complexConvert (ZX.CComp (ZX.Scalar s1) (ZX.Scalar s2)) = read s1 :+ read s2
complexConvert (ZX.CComn (ZX.Scalar s1) (ZX.Scalar s2)) = read s1 :+ negate (read s2)
complexConvert ZX.CPi = pi :+ 0
complexConvert ZX.CE = exp 1 :+ 0
complexConvert (ZX.CExp c1 c2) = complexConvert c1**complexConvert c2
complexConvert (ZX.CDiv c1 c2) = complexConvert c1/complexConvert c2
complexConvert (ZX.CMul c1 c2) = complexConvert c1*complexConvert c2

convertId :: ZX.Id -> IAST.Id
convertId (ZX.Id ((x, y), name)) = IAST.Id (x,y) name

convertIds :: [ZX.Id] -> [IAST.Id]
convertIds = map convertId

revert :: IAST.Program -> ZX.Program
revert (IAST.Progr topls) = ZX.Progr $ map revertTopl topls

revertTopl :: IAST.Toplevel -> ZX.Toplevel
revertTopl (IAST.ToplF name ids e) = ZX.ToplF (revertId name) (revertIds ids) (revertExpr e)
revertTopl (IAST.Topl IAST.Z c ids e) = ZX.ToplZ (revertComplex c) (revertIds ids) (revertExpr e)
revertTopl (IAST.Topl IAST.X c ids e) = ZX.ToplX (revertComplex c) (revertIds ids) (revertExpr e)
revertTopl (IAST.Topl IAST.None c ids e) = ZX.ToplF ((revertId . head) ids) (revertIds ids) (revertExpr e)

revertExpr :: IAST.Expr -> ZX.Expr
revertExpr (IAST.Var var) = ZX.EVar (revertId var)
revertExpr (IAST.Tup e es) = ZX.ETup (revertExpr e) (map revertExpr es)
revertExpr IAST.Unit = ZX.EUnit
revertExpr (IAST.App e1 e2) = ZX.EApp (revertExpr e1) (revertExpr e2)
revertExpr (IAST.Comp e1 e2) = ZX.EComp (revertExpr e1) (revertExpr e2)
revertExpr (IAST.Had e) = ZX.EHad (revertExpr e)
revertExpr (IAST.Abs IAST.X c ids e) = ZX.EXAbs (revertComplex c) (revertIds ids) (revertExpr e)
revertExpr (IAST.Abs IAST.Z c ids e) = ZX.EZAbs (revertComplex c) (revertIds ids) (revertExpr e)
revertExpr (IAST.Abs _ _ ids e) = ZX.EAbs (revertIds ids) (revertExpr e)

revertComplex :: Complex Double -> ZX.Complex
revertComplex (a :+ b) = ZX.CComp (rtoStr a) (rtoStr b)
    where rtoStr r = ZX.Scalar (show r)

revertId :: IAST.Id -> ZX.Id
revertId (IAST.Id pos name) = ZX.Id (pos, name)

revertIds :: [IAST.Id] -> [ZX.Id]
revertIds = map revertId

testZXPrg :: ZX.Program
testZXPrg = ZX.Progr [topl]
    where topl = ZX.ToplX toplC ids expr
          toplC = ZX.CComp (ZX.Scalar "1") (ZX.Scalar "0")
          absC = ZX.CComp (ZX.Scalar "0.5") (ZX.Scalar "0")
          expr = ZX.EApp (ZX.EXAbs absC ids ZX.EUnit) (ZX.EComp (ZX.EVar id1) (ZX.EVar id2))
          id1 = ZX.Id ((0,0), "id1")
          id2 = ZX.Id ((0,1), "id2")
          ids = [id1, id2]

testIASTPrg :: IAST.Program
testIASTPrg = IAST.Progr [topl]
    where topl = IAST.Topl IAST.Z (0 :+ 0) ids expr
          expr = IAST.App (IAST.Abs IAST.Z (0 :+ 0) [id1] (IAST.Had (IAST.Var id1)))
                (IAST.Var id2)
          id1 = IAST.Id (0,0) "id1"
          id2 = IAST.Id (0,1) "id2"
          ids = [id1,id2]
