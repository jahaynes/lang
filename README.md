# lang
Programming language implementation

# example output

cat progs/list_summorial.src | ./lang

```
Raw:

data List a = Nil
            | Cons a List

genList n =
  let genListGo acc a b =
    if a > b
      then acc
      else genListGo (Cons a acc) (a + 1) b in
  genListGo Nil 0 n

sumList xs = 
  let sumListGo acc ys =
        ys acc
           (\h t. sumListGo (h + acc) t) in
  sumListGo 0 xs

main =
  let ls = genList 10
  in
  sumList ls
 * Tokens (Positional information elided) *
[TData,UpperIdent "List",LowerIdent "a",SingleEq,UpperIdent "Nil",TPipe,UpperIdent "Cons",LowerIdent "a",UpperIdent "List",LowerIdent "genList",LowerIdent "n",SingleEq,TLet,LowerIdent "genListGo",LowerIdent "acc",LowerIdent "a",LowerIdent "b",SingleEq,If,LowerIdent "a",TGreaterThan,LowerIdent "b",Then,LowerIdent "acc",Else,LowerIdent "genListGo",LParen,UpperIdent "Cons",LowerIdent "a",LowerIdent "acc",RParen,LParen,LowerIdent "a",Plus,TokInt 1,RParen,LowerIdent "b",In,LowerIdent "genListGo",UpperIdent "Nil",TokInt 0,LowerIdent "n",LowerIdent "sumList",LowerIdent "xs",SingleEq,TLet,LowerIdent "sumListGo",LowerIdent "acc",LowerIdent "ys",SingleEq,LowerIdent "ys",LowerIdent "acc",LParen,Lambda,LowerIdent "h",LowerIdent "t",Dot,LowerIdent "sumListGo",LParen,LowerIdent "h",Plus,LowerIdent "acc",RParen,LowerIdent "t",RParen,In,LowerIdent "sumListGo",TokInt 0,LowerIdent "xs",LowerIdent "main",SingleEq,TLet,LowerIdent "ls",SingleEq,LowerIdent "genList",TokInt 10,In,LowerIdent "sumList",LowerIdent "ls"]

 * Top-level definitions *
DataDef (TC "List" ["a"]) [DataCons "Nil" [],DataCons "Cons" ["a",TC "List" []]]
FunDef "genList" (ELam "n" (ELet "genListGo" (ELam "acc" (ELam "a" (ELam "b" (IfThenElse (EBinPrimOp GtI (ETerm (Var "a")) (ETerm (Var "b"))) (ETerm (Var "acc")) (EApp (EApp (EApp (ETerm (Var "genListGo")) (EApp (EApp (ETerm (DCons "Cons")) (ETerm (Var "a"))) (ETerm (Var "acc")))) (EBinPrimOp AddI (ETerm (Var "a")) (ETerm (LitInt 1)))) (ETerm (Var "b"))))))) (EApp (EApp (EApp (ETerm (Var "genListGo")) (ETerm (DCons "Nil"))) (ETerm (LitInt 0))) (ETerm (Var "n")))))
FunDef "sumList" (ELam "xs" (ELet "sumListGo" (ELam "acc" (ELam "ys" (EApp (EApp (ETerm (Var "ys")) (ETerm (Var "acc"))) (ELam "h" (ELam "t" (EApp (EApp (ETerm (Var "sumListGo")) (EBinPrimOp AddI (ETerm (Var "h")) (ETerm (Var "acc")))) (ETerm (Var "t")))))))) (EApp (EApp (ETerm (Var "sumListGo")) (ETerm (LitInt 0))) (ETerm (Var "xs")))))
FunDef "main" (ELet "ls" (EApp (ETerm (Var "genList")) (ETerm (LitInt 10))) (EApp (ETerm (Var "sumList")) (ETerm (Var "ls"))))

 * Top-level definitions (alphabetised) *
DataDef (TC "List" ["a"]) [DataCons "Nil" [],DataCons "Cons" ["a",TC "List" []]]
FunDef "genList" (ELam "v1" (ELet "v2" (ELam "v3" (ELam "v4" (ELam "v5" (IfThenElse (EBinPrimOp GtI (ETerm (Var "v4")) (ETerm (Var "v5"))) (ETerm (Var "v3")) (EApp (EApp (EApp (ETerm (Var "v2")) (EApp (EApp (ETerm (DCons "Cons")) (ETerm (Var "v4"))) (ETerm (Var "v3")))) (EBinPrimOp AddI (ETerm (Var "v4")) (ETerm (LitInt 1)))) (ETerm (Var "v5"))))))) (EApp (EApp (EApp (ETerm (Var "v2")) (ETerm (DCons "Nil"))) (ETerm (LitInt 0))) (ETerm (Var "v1")))))
FunDef "sumList" (ELam "v6" (ELet "v7" (ELam "v8" (ELam "v9" (EApp (EApp (ETerm (Var "v9")) (ETerm (Var "v8"))) (ELam "v10" (ELam "v11" (EApp (EApp (ETerm (Var "v7")) (EBinPrimOp AddI (ETerm (Var "v10")) (ETerm (Var "v8")))) (ETerm (Var "v11")))))))) (EApp (EApp (ETerm (Var "v7")) (ETerm (LitInt 0))) (ETerm (Var "v6")))))
FunDef "main" (ELet "v12" (EApp (ETerm (Var "genList")) (ETerm (LitInt 10))) (EApp (ETerm (Var "sumList")) (ETerm (Var "v12"))))

 * Source map *
("v1","n")
("v10","h")
("v11","t")
("v12","ls")
("v2","genListGo")
("v3","acc")
("v4","a")
("v5","b")
("v6","xs")
("v7","sumListGo")
("v8","acc")
("v9","ys")

 * Top-level environment with data constructors *
"Nil": ELam "List_0" (ELam "List_1" (ETerm (Var "List_0")))
"Cons": ELam "Cons_2" (ELam "Cons_3" (ELam "List_0" (ELam "List_1" (EApp (EApp (ETerm (Var "List_1")) (ETerm (Var "Cons_2"))) (ETerm (Var "Cons_3"))))))
"genList": ELam "v1" (ELet "v2" (ELam "v3" (ELam "v4" (ELam "v5" (IfThenElse (EBinPrimOp GtI (ETerm (Var "v4")) (ETerm (Var "v5"))) (ETerm (Var "v3")) (EApp (EApp (EApp (ETerm (Var "v2")) (EApp (EApp (ETerm (DCons "Cons")) (ETerm (Var "v4"))) (ETerm (Var "v3")))) (EBinPrimOp AddI (ETerm (Var "v4")) (ETerm (LitInt 1)))) (ETerm (Var "v5"))))))) (EApp (EApp (EApp (ETerm (Var "v2")) (ETerm (DCons "Nil"))) (ETerm (LitInt 0))) (ETerm (Var "v1"))))
"sumList": ELam "v6" (ELet "v7" (ELam "v8" (ELam "v9" (EApp (EApp (ETerm (Var "v9")) (ETerm (Var "v8"))) (ELam "v10" (ELam "v11" (EApp (EApp (ETerm (Var "v7")) (EBinPrimOp AddI (ETerm (Var "v10")) (ETerm (Var "v8")))) (ETerm (Var "v11")))))))) (EApp (EApp (ETerm (Var "v7")) (ETerm (LitInt 0))) (ETerm (Var "v6"))))
"main": ELet "v12" (EApp (ETerm (Var "genList")) (ETerm (LitInt 10))) (EApp (ETerm (Var "sumList")) (ETerm (Var "v12")))

 * Top-level environment with data constructors (pretty) *
Nil: (\List_0. (\List_1. List_0))
Cons: (\Cons_2. (\Cons_3. (\List_0. (\List_1. ((List_1 Cons_2) Cons_3)))))
genList: (\n. (let genListGo = (\acc. (\a. (\b. (if (> a b) then acc else (((genListGo ((Cons a) acc)) (+ a 1)) b))))) in (((genListGo Nil) 0) n)))
sumList: (\xs. (let sumListGo = (\acc. (\ys. ((ys acc) (\h. (\t. ((sumListGo (+ h acc)) t)))))) in ((sumListGo 0) xs)))
main: (let ls = (genList 10) in (sumList ls))


 * Lifting recursive lambdas *
("Looking at: ","Nil")
    No nested recursions

("Looking at: ","Cons")
    No nested recursions

("Looking at: ","genList")
    Lifting out: 1 definitions

("Looking at: ","sumList")
    Lifting out: 1 definitions

("Looking at: ","main")
    No nested recursions


 * Lambda lift resulted in (pretty) *
Nil: (\List_0. (\List_1. List_0))
Cons: (\Cons_2. (\Cons_3. (\List_0. (\List_1. ((List_1 Cons_2) Cons_3)))))
genList: (\n. ((((genListGo n) Nil) 0) n))
genListGo: (\n. (\acc. (\a. (\b. (if (> a b) then acc else ((((genListGo n) ((Cons a) acc)) (+ a 1)) b))))))
sumList: (\xs. (((sumListGo xs) 0) xs))
sumListGo: (\xs. (\acc. (\ys. ((ys acc) (\h. (\t. (((sumListGo xs) (+ h acc)) t)))))))
main: (let ls = (genList 10) in (sumList ls))


 * Main declaration located *

 * Evaluation *
ETerm (LitInt 55)

 * Evaluation (Pretty) *
55

 * Will try to CPS *

 * Transformed to: *
("main",CFix [(VVar "v7",[VVar "v6",VVar "var0"],CFix [(VVar "var19",[VVar "v8",VVar "var1"],CFix [(VVar "var18",[VVar "v9",VVar "var2"],CFix [(VVar "var3",[VVar "var17"],CApp (VVar "var2") [VVar "var17"])] (CFix [(VVar "var4",[VVar "var5"],CFix [(VVar "var16",[VVar "v10",VVar "var6"],CFix [(VVar "var15",[VVar "v11",VVar "var7"],CFix [(VVar "var8",[VVar "var14"],CApp (VVar "var7") [VVar "var14"])] (CFix [(VVar "var9",[VVar "var13"],CApp (VVar "var13") [VVar "v11",VVar "var8"])] (CFix [(VVar "var10",[VVar "var11"],CPrimOp AddI [VVar "v10",VVar "v8"] [VVar "var12"] [CApp (VVar "var11") [VVar "var12",VVar "var9"]])] (CApp (VVar "v7") [VVar "v6",VVar "var10"]))))] (CApp (VVar "var6") [VVar "var15"]))] (CApp (VVar "var5") [VVar "var16",VVar "var3"]))] (CApp (VVar "v9") [VVar "v8",VVar "var4"])))] (CApp (VVar "var1") [VVar "var18"]))] (CApp (VVar "var0") [VVar "var19"])),(VVar "sumList",[VVar "v6",VVar "var20"],CFix [(VVar "var21",[VVar "var26"],CApp (VVar "var20") [VVar "var26"])] (CFix [(VVar "var22",[VVar "var25"],CApp (VVar "var25") [VVar "v6",VVar "var21"])] (CFix [(VVar "var23",[VVar "var24"],CApp (VVar "var24") [VInt 0,VVar "var22"])] (CApp (VVar "v7") [VVar "v6",VVar "var23"])))),(VVar "v2",[VVar "v1",VVar "var27"],CFix [(VVar "var47",[VVar "v3",VVar "var28"],CFix [(VVar "var46",[VVar "v4",VVar "var29"],CFix [(VVar "var45",[VVar "v5",VVar "var30"],CPrimOp GtI [VVar "v4",VVar "v5"] [VVar "var31"] [CSwitch (VVar "var31") (CApp (VVar "var30") [VVar "v3"]) (CFix [(VVar "var32",[VVar "var44"],CApp (VVar "var30") [VVar "var44"])] (CFix [(VVar "var33",[VVar "var43"],CApp (VVar "var43") [VVar "v5",VVar "var32"])] (CFix [(VVar "var34",[VVar "var41"],CPrimOp AddI [VVar "v4",VInt 1] [VVar "var42"] [CApp (VVar "var41") [VVar "var42",VVar "var33"]])] (CFix [(VVar "var35",[VVar "var36"],CFix [(VVar "var37",[VVar "var40"],CApp (VVar "var36") [VVar "var40",VVar "var34"])] (CFix [(VVar "var38",[VVar "var39"],CApp (VVar "var39") [VVar "v3",VVar "var37"])] (CApp (VVar "Cons") [VVar "v4",VVar "var38"])))] (CApp (VVar "v2") [VVar "v1",VVar "var35"])))))])] (CApp (VVar "var29") [VVar "var45"]))] (CApp (VVar "var28") [VVar "var46"]))] (CApp (VVar "var27") [VVar "var47"])),(VVar "genList",[VVar "v1",VVar "var48"],CFix [(VVar "var49",[VVar "var56"],CApp (VVar "var48") [VVar "var56"])] (CFix [(VVar "var50",[VVar "var55"],CApp (VVar "var55") [VVar "v1",VVar "var49"])] (CFix [(VVar "var51",[VVar "var54"],CApp (VVar "var54") [VInt 0,VVar "var50"])] (CFix [(VVar "var52",[VVar "var53"],CApp (VVar "var53") [VVar "Nil",VVar "var51"])] (CApp (VVar "v2") [VVar "v1",VVar "var52"]))))),(VVar "Cons",[VVar "Cons_2",VVar "var57"],CFix [(VVar "var67",[VVar "Cons_3",VVar "var58"],CFix [(VVar "var66",[VVar "List_0",VVar "var59"],CFix [(VVar "var65",[VVar "List_1",VVar "var60"],CFix [(VVar "var61",[VVar "var64"],CApp (VVar "var60") [VVar "var64"])] (CFix [(VVar "var62",[VVar "var63"],CApp (VVar "var63") [VVar "Cons_3",VVar "var61"])] (CApp (VVar "List_1") [VVar "Cons_2",VVar "var62"])))] (CApp (VVar "var59") [VVar "var65"]))] (CApp (VVar "var58") [VVar "var66"]))] (CApp (VVar "var57") [VVar "var67"])),(VVar "Nil",[VVar "List_0",VVar "var68"],CFix [(VVar "var70",[VVar "List_1",VVar "var69"],CApp (VVar "var69") [VVar "List_0"])] (CApp (VVar "var68") [VVar "var70"]))] (CFix [(VVar "var71",[VVar "var78"],CHalt (VVar "var78"))] (CFix [(VVar "var75",[VVar "v12",VVar "var72"],CFix [(VVar "var73",[VVar "var74"],CApp (VVar "var72") [VVar "var74"])] (CApp (VVar "sumList") [VVar "v12",VVar "var73"]))] (CFix [(VVar "var76",[VVar "var77"],CApp (VVar "var75") [VVar "var77",VVar "var71"])] (CApp (VVar "genList") [VInt 10,VVar "var76"])))))

 * Toplevels evaluated to: *

 * Will try to Cps Eval: *
Answer DInt 55
```

