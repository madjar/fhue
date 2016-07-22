import           Test.Tasty
import           Test.Tasty.HUnit

import           Shell
-- TODO try out hspec, and choose between pure hspec and hspec tasty

--main = defaultMain tests
main = runFakeHdfs runShell

tests :: TestTree
tests = testGroup "Tests"
 [
   testCase "True is True" $
     True @?= True


 ]
