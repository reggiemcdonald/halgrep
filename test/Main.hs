import Test.Hspec.Runner
import qualified Spec

{-
  Use of hspec was learned from https://github.com/iustin/corydalis/blob/dfbc654a0aa509b56736ba073789b2967d8c3aba/test/Main.hs
-}
main :: IO ()
main = do 
  hspec Spec.spec