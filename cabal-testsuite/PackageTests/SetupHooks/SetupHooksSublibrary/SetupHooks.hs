{-# LANGUAGE DataKinds, DuplicateRecordFields, StaticPointers #-}
module SetupHooks (setupHooks) where
import qualified Data.List.NonEmpty as NE
import Distribution.Simple.SetupHooks
import Distribution.Simple.LocalBuildInfo (mbWorkDirLBI)
import Distribution.Utils.Path
import Distribution.Utils.ShortText (toShortText)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

setupHooks :: SetupHooks
setupHooks = noSetupHooks { buildHooks = noBuildHooks { preBuildComponentRules = Just $ rules (static ()) genB } }

genB :: PreBuildComponentInputs -> RulesM ()
genB PreBuildComponentInputs{localBuildInfo = lbi, targetInfo = tgt} =
  case targetComponent tgt of
    CLib lib | libName lib /= LMainLibName ->
      let out = Location (autogenComponentModulesDir lbi (targetCLBI tgt)) (makeRelativePathEx "B.hs")
       in registerRule_ (toShortText "B") $
            staticRule (mkCommand (static Dict) (static writeB) (mbWorkDirLBI lbi, out)) [] (out NE.:| [])
    _ -> pure ()

writeB :: (Maybe (SymbolicPath CWD (Dir Pkg)), Location) -> IO ()
writeB (wd, out) = do
  let p = interpretSymbolicPath wd (location out)
  createDirectoryIfMissing True (takeDirectory p)
  writeFile p "module B where\nb :: Int\nb = 41\n"
