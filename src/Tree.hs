module Tree (
    MetaInformation(..),
    DirectoryTree(..),
    buildTree, printTree
) where

import Prelude hiding (ReaderT)

import Data.ByteString.Lazy qualified as BSL

import Crypto.Hash.MD5 qualified as MD5
import Control.Concurrent.Async.Pool
import System.Directory
import System.FilePath ((</>), takeFileName)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT))
import Control.Monad.Writer.Class qualified as MTL
import Control.Monad.RWS (MonadWriter)
import Control.Exception (try, IOException)
import Data.Tree (drawTree, Tree (Node))
import Data.ByteString.Base64 (encodeBase64)

-- | Entry meta information.
data MetaInformation a = Meta {
    path :: !Text, -- ^ Name of entry (file or directory).
    hash :: !a     -- ^ SHA256 hash of entry (file or directory).
  } deriving stock (Show, Eq, Ord, Functor)

data DirectoryTree a
  = File (MetaInformation a) -- ^ File meta-information.
  -- | Directory meta-information. Entries represented
  -- | as DirectoryTree.
  | Directory (MetaInformation ()) (Set (DirectoryTree a))
  deriving stock (Show, Eq, Ord)

hashFile :: FilePath -> IO ByteString
{-# inline hashFile #-}
hashFile path = do
    file <- BSL.readFile path
    let hash = MD5.hashlazy file
    return hash

type SpawnedTree = DirectoryTree (Async ByteString)
type Spawn = MaybeT (WriterT [Text] IO) SpawnedTree

runSpawn :: Spawn -> IO ([Text], SpawnedTree)
{-# inlinable runSpawn #-}
runSpawn s = do
    (t, e) <- runWriterT $ runMaybeT s
    return (e, t ?: Directory (Meta "" ()) mempty)

handleIO :: (MonadIO m, MonadWriter [Text] m) => IO a -> MaybeT m a
{-# inline handleIO #-}
handleIO action = do
    r <- liftIO $ (try @IOException) action
    case r of
        Right r -> hoistMaybe $ Just r
        Left e -> do
            MTL.tell [show e]
            hoistMaybe Nothing

spawnHash :: TaskGroup -> FilePath -> Spawn
{-# inline spawnHash #-}
spawnHash taskGroup path = do
    isFile <- handleIO $ doesFileExist path
    isDirectory <- handleIO $ doesDirectoryExist path

    if isFile then do
        hash <- handleIO $ async taskGroup (hashFile path)
        hoistMaybe $ Just $ File (Meta (toText path) hash)
    else if isDirectory then do
        paths <- handleIO $ listDirectory path
        spawned <- lift $ mapM (\e -> runMaybeT (spawnHash taskGroup $ path </> e)) paths
        hoistMaybe $ Just $ Directory (Meta (toText path) ()) (fromList $ catMaybes spawned)
    else do
        MTL.tell ["Failed to find data on FS: " <> toText path]
        hoistMaybe Nothing

waitHash :: SpawnedTree -> IO (DirectoryTree ByteString)
{-# inline waitHash #-}
waitHash = \case
   File (Meta n h) -> File . Meta n <$> wait h
   Directory m es -> do
      es' <- mapM waitHash (toList es)
      return $ Directory m (fromList es')

buildTree :: Int -> FilePath -> IO ([Text], DirectoryTree ByteString)
{-# inlinable buildTree #-}
buildTree threads path = do
    withTaskGroup threads $ \taskGroup -> do
        (errors, spawned) <- runSpawn $ spawnHash taskGroup path
        awaited <- waitHash spawned
        return (errors, awaited)

printTree :: DirectoryTree ByteString -> String
{-# inlinable printTree #-}
printTree = drawTree . convert
  where
    convert = \case
      File (Meta n h) -> Node (file n h) []
      Directory (Meta n _) entries -> Node (takeFileName $ toString n) (convert <$> toList entries)

    file path hash = let
            filename = takeFileName $ toString path
            shortHash = take 8 . toString . encodeBase64 $ hash
        in
            shortHash <> ": " <> filename
