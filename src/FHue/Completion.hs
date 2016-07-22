module FHue.Completion (ailCompletion) where

import           Control.Lens
import           Control.Monad.Trans
import           Data.List
import           System.Console.Haskeline.Completion
import           System.FilePath

import           FHue.AilTypes
import           FHue.Types

ailCompletion :: MonadHdfs m => CompletionFunc (Ail m)
ailCompletion = completeWord (Just '\\') filenameWordBreakChars complete

-- TODO, redo this after refacto
complete :: MonadHdfs m => String -> (Ail m) [Completion]
--complete = undefined
complete path = do current <- use currentDirectory
                   result <- lift $ list (current </> takeDirectory path)
                   let files = filter ((path `isPrefixOf`) . itemName) result
                       completions = map toCompletion files
                   return completions
  where toCompletion item = alterIfDir (itemType item) $ Completion (itemName item) (takeFileName $ itemName item) True
        alterIfDir File c = c
        alterIfDir Dir c = c {replacement = addTrailingPathSeparator (replacement c),
                                       isFinished = False}
