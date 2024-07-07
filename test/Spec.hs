import Lib (execFile)
import System.FilePath ((<.>), (</>))

main :: IO ()
main = do
  let fileName = "bst/bst"
  execFile $ "test" </> "data" </> fileName <.> "adpl"