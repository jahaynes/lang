module AsmWriter where

import CpsTypes

import Control.Monad         (when)
import Data.ByteString.Char8 (ByteString, unpack)
-- import Data.List             (intercalate)

import           Data.Map      (Map)
import qualified Data.Map as M

import Text.Printf
import Control.Monad.State   (StateT, execStateT, get, modify', put)

--TODO pull out strings

data WriteState =
    WriteState { _comments            :: !Bool
               , _getN                :: !Int
               , _getFreeRegs         :: ![String]
               , _getRegisterBindings :: !(Map String String)
               , _getLabels           :: ![String]
               , _output              :: ![String]
               }

intro :: String
intro = ".text \n\
        \.global main\n\n"

outro :: String
outro = "    bx lr\n"

-- no safety checks
bindRegister :: Monad m => Val ByteString -> StateT WriteState m (Maybe String)
bindRegister v@(VInt _) = pure . Just . un $ v
bindRegister (VVar v) = do
    let var = unpack v
    s <- get

    case M.lookup var (_getRegisterBindings s) of
        Just r -> pure $ Just r
        Nothing ->
            case _getFreeRegs s of
                [] -> pure Nothing
                (r:rs) -> do
                    put s { _getFreeRegs         = rs
                          , _getRegisterBindings = M.insert var r (_getRegisterBindings s)
                          }
                    pure $ Just r
bindRegister x = error $ show x

-- warn no dupe check
-- err no unbinding
releaseReg :: Monad m => String -> StateT WriteState m ()
releaseReg reg = modify' $ \s -> s { _getFreeRegs = reg : _getFreeRegs s }

writeAsm :: Cexp ByteString -> IO ()
writeAsm x = do

    print x

    putStrLn "\n---------------------------------------\n"

    putStrLn =<< mconcat . reverse . _output
             <$> execStateT (write intro >> label' "main" >> emit x)
                            (WriteState False 0 ["r1","r2","r3","r4","r5","r6"] mempty ["main"] [])

emit :: MonadFail m => Cexp ByteString -> StateT WriteState m ()

emit (CFix funs k) = do

    comment $ printf "instructions for fix [%s]:" (unwords . map (\(f,_,_) -> un f) $ funs)

    emit k

    mapM_ emitFun funs

emit (CApp (VVar fun) params) = do
    push params
    write $ printf "    b %s\n" (unpack fun)

emit (CHalt v) = do
    comment $ printf "instructions for halt:"    
    write $ printf "    mov r0, %s\n" (un v)
    write outro

    --Just l <- popLabel
    --label' l

emit (CPrimOp op [p1, p2] dest k) = do

    comment $ printf "instructions for primop %s:" (show op)

    case op of

        EqI -> do
            -- clear bool to 0
            write $ printf "    mov %s, #0\n" (un dest)
            write $ printf "    cmp %s, %s\n" (un p1) (un p2)
            write $ printf "    moveq %s, #1\n" (un dest)

        GtI -> do
            -- clear bool to 0
            write $ printf "    mov %s, #0\n" (un dest)
            write $ printf "    cmp %s, %s\n" (un p1) (un p2)
            write $ printf "    movgt %s, #1\n" (un dest)

        AddI -> do
            write $ printf "    add %s, %s, %s\n" (un dest) (un p1) (un p2)

        x -> error $ show x

    emit k

emit (CSwitch b t f) = do
    comment $ printf "instructions for switch:"
    write $ printf "    cmp %s, %s\n" (un b) "#1"
    n' <- getAndIncN
    write $ printf "    beq iftrue_%d\n" n'
    write $ printf "    b iffalse_%d\n" n'
    label' $ printf "iftrue_%d" n'
    emit t
    label' $ printf "iffalse_%d" n'
    emit f

emit x = error $ show x

emitFun :: MonadFail m => (Val ByteString, [Val ByteString], Cexp ByteString)
                       -> StateT WriteState m ()
emitFun (name, params, body) = do
    comment $ printf "instructions for emitFun:"
    label name   
    pop params
    emit body

emitOp :: COp -> String
emitOp AddI = "+"
emitOp GtI  = ">"
emitOp x = error $ show x

un :: Val ByteString -> String
un (VInt i)    = printf "#%d" i
un (VVar v)    = unpack v
un (VString s) = printf "\"%s\"" (unpack s)
un x = error $ show x

getAndIncN :: Monad m => StateT WriteState m Int
getAndIncN = do
    s <- get
    let n = _getN s
    put $ s { _getN = n + 1}
    pure n

getN :: Monad m => StateT WriteState m Int
getN = _getN <$> get

write :: Monad m => String -> StateT WriteState m ()
write x = modify' $ \s -> s { _output = x : _output s }

label :: Monad m => Val ByteString -> StateT WriteState m ()
label = label' . un

label' :: Monad m => String -> StateT WriteState m ()
label' = write . printf "%s:\n"

pop :: Monad m => [Val ByteString] -> StateT WriteState m ()
--pop params = write $ "    pop {" ++ (intercalate ", " . map un $ params) ++ "}\n"
pop = mapM_ (write . printf "    pop {%s}\n" . un)  


push :: Monad m => [Val ByteString] -> StateT WriteState m ()
--push params = write $ "    push {" ++ (intercalate ", " . map un $ params) ++ "}\n"
push = mapM_ (write . printf "    push {%s}\n" . un) . reverse -- pops are in reverse order

comment :: Monad m => String -> StateT WriteState m ()
comment c = do
    comments <- _comments <$> get
    when comments . write . printf "@ %s\n" $ c
