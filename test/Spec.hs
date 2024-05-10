import Lib (execFile)
import System.FilePath ((<.>), (</>))

main :: IO ()
main = do
  let fileName = "bin_tree"
  execFile $ "test" </> "data" </> fileName <.> "adpl"