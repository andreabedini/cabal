{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Repository.Cache
  ( Cache (..)
  , IndexCacheEntry (..)
  , PackageEntry (..)
  , BlockNo
  , cacheEntryTimestamp
  , packageDesc
  , typeCodeFromRefType
  , refTypeFromTypeCode
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Codec.Archive.Tar.Entry as Tar

import Data.ByteString.Lazy (ByteString)

import Distribution.Client.IndexUtils.Timestamp (Timestamp, nullTimestamp)
import qualified Distribution.Client.Tar as Tar
import Distribution.Package (Package (..))
import Distribution.PackageDescription

--
-- Cache structure
--

-- | Cabal caches various information about the Hackage index
data Cache = Cache
  { cacheHeadTs :: Timestamp
  -- ^ maximum/latest 'Timestamp' among 'cacheEntries'; unless the
  -- invariant of 'cacheEntries' being in chronological order is
  -- violated, this corresponds to the last (seen) 'Timestamp' in
  -- 'cacheEntries'
  , cacheEntries :: [IndexCacheEntry]
  }
  deriving (Show, Generic)

instance Binary Cache
instance Structured Cache

instance NFData Cache where
  rnf = rnf . cacheEntries

data IndexCacheEntry
  = CachePackageId PackageId !BlockNo !Timestamp
  | CachePreference Dependency !BlockNo !Timestamp
  | CacheBuildTreeRef !BuildTreeRefType !BlockNo
  -- NB: CacheBuildTreeRef is irrelevant for 01-index & v2-build
  deriving (Eq, Show, Generic)

instance Binary IndexCacheEntry
instance Structured IndexCacheEntry

instance NFData IndexCacheEntry where
  rnf (CachePackageId pkgid _ _) = rnf pkgid
  rnf (CachePreference dep _ _) = rnf dep
  rnf (CacheBuildTreeRef _ _) = ()

-- | Tar files are block structured with 512 byte blocks. Every header and file
-- content starts on a block boundary.
type BlockNo = Word32 -- Tar.TarEntryOffset

cacheEntryTimestamp :: IndexCacheEntry -> Timestamp
cacheEntryTimestamp (CacheBuildTreeRef _ _) = nullTimestamp
cacheEntryTimestamp (CachePreference _ _ ts) = ts
cacheEntryTimestamp (CachePackageId _ _ ts) = ts

-- | An index entry is either a normal package, or a local build tree reference.
data PackageEntry
  = NormalPackage PackageId GenericPackageDescription ByteString BlockNo
  | BuildTreeRef
      BuildTreeRefType
      PackageId
      GenericPackageDescription
      FilePath
      BlockNo

instance Package PackageEntry where
  packageId (NormalPackage pkgid _ _ _) = pkgid
  packageId (BuildTreeRef _ pkgid _ _ _) = pkgid

packageDesc :: PackageEntry -> GenericPackageDescription
packageDesc (NormalPackage _ descr _ _) = descr
packageDesc (BuildTreeRef _ _ descr _ _) = descr

-- | A build tree reference is either a link or a snapshot.
data BuildTreeRefType = SnapshotRef | LinkRef
  deriving (Eq, Show, Generic)

instance Binary BuildTreeRefType
instance Structured BuildTreeRefType

refTypeFromTypeCode :: Tar.TypeCode -> BuildTreeRefType
refTypeFromTypeCode t
  | t == Tar.buildTreeRefTypeCode = LinkRef
  | t == Tar.buildTreeSnapshotTypeCode = SnapshotRef
  | otherwise =
      error "Distribution.Client.IndexUtils.refTypeFromTypeCode: unknown type code"

typeCodeFromRefType :: BuildTreeRefType -> Tar.TypeCode
typeCodeFromRefType LinkRef = Tar.buildTreeRefTypeCode
typeCodeFromRefType SnapshotRef = Tar.buildTreeSnapshotTypeCode

-- --
-- -- legacy 00-index.cache format
-- --
--
-- packageKey, blocknoKey, buildTreeRefKey, preferredVersionKey :: String
-- packageKey = "pkg:"
-- blocknoKey = "b#"
-- buildTreeRefKey = "build-tree-ref:"
-- preferredVersionKey = "pref-ver:"
--
-- read00IndexCache :: BSS.ByteString -> Cache
-- read00IndexCache bs =
--   Cache
--     { cacheHeadTs = nullTimestamp
--     , cacheEntries = mapMaybe read00IndexCacheEntry $ BSS.lines bs
--     }
--
-- read00IndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
-- read00IndexCacheEntry = \line ->
--   case BSS.words line of
--     [key, pkgnamestr, pkgverstr, sep, blocknostr]
--       | key == BSS.pack packageKey && sep == BSS.pack blocknoKey ->
--           case ( parseName pkgnamestr
--                , parseVer pkgverstr []
--                , parseBlockNo blocknostr
--                ) of
--             (Just pkgname, Just pkgver, Just blockno) ->
--               Just
--                 ( CachePackageId
--                     (PackageIdentifier pkgname pkgver)
--                     blockno
--                     nullTimestamp
--                 )
--             _ -> Nothing
--     [key, typecodestr, blocknostr] | key == BSS.pack buildTreeRefKey ->
--       case (parseRefType typecodestr, parseBlockNo blocknostr) of
--         (Just refType, Just blockno) ->
--           Just (CacheBuildTreeRef refType blockno)
--         _ -> Nothing
--     (key : remainder) | key == BSS.pack preferredVersionKey -> do
--       pref <- simpleParsecBS (BSS.unwords remainder)
--       return $ CachePreference pref 0 nullTimestamp
--     _ -> Nothing
--   where
--     parseName str
--       | BSS.all (\c -> isAlphaNum c || c == '-') str =
--           Just (mkPackageName (BSS.unpack str))
--       | otherwise = Nothing
--
--     parseVer str vs =
--       case BSS.readInt str of
--         Nothing -> Nothing
--         Just (v, str') -> case BSS.uncons str' of
--           Just ('.', str'') -> parseVer str'' (v : vs)
--           Just _ -> Nothing
--           Nothing -> Just (mkVersion (reverse (v : vs)))
--
--     parseBlockNo str =
--       case BSS.readInt str of
--         Just (blockno, remainder)
--           | BSS.null remainder -> Just (fromIntegral blockno)
--         _ -> Nothing
--
--     parseRefType str =
--       case BSS.uncons str of
--         Just (typeCode, remainder)
--           | BSS.null remainder && Tar.isBuildTreeRefTypeCode typeCode ->
--               Just (refTypeFromTypeCode typeCode)
--         _ -> Nothing
--
-- show00IndexCache :: Cache -> String
-- show00IndexCache Cache{..} = unlines $ map show00IndexCacheEntry cacheEntries
--
-- show00IndexCacheEntry :: IndexCacheEntry -> String
-- show00IndexCacheEntry entry = unwords $ case entry of
--   CachePackageId pkgid b _ ->
--     [ packageKey
--     , prettyShow (packageName pkgid)
--     , prettyShow (packageVersion pkgid)
--     , blocknoKey
--     , show b
--     ]
--   CacheBuildTreeRef tr b ->
--     [ buildTreeRefKey
--     , [typeCodeFromRefType tr]
--     , show b
--     ]
--   CachePreference dep _ _ ->
--     [ preferredVersionKey
--     , prettyShow dep
--     ]
--
-- --
-- -- Reading the index file
-- --
--
-- -- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- -- as a 'ByteString'.
-- data PackageOrDep = Pkg PackageEntry | Dep Dependency
--
-- -- | Read @00-index.tar.gz@ and extract @.cabal@ and @preferred-versions@ files
-- --
-- -- We read the index using 'Tar.read', which gives us a lazily constructed
-- -- 'TarEntries'. We translate it to a list of entries using  'tarEntriesList',
-- -- which preserves the lazy nature of 'TarEntries', and finally 'concatMap' a
-- -- function over this to translate it to a list of IO actions returning
-- -- 'PackageOrDep's. We can use 'lazySequence' to turn this into a list of
-- -- 'PackageOrDep's, still maintaining the lazy nature of the original tar read.
-- parsePackageIndex :: Verbosity -> ByteString -> [IO (Maybe PackageOrDep)]
-- parsePackageIndex verbosity = concatMap (uncurry extract) . tarEntriesList . Tar.read
--   where
--     extract :: BlockNo -> Tar.Entry -> [IO (Maybe PackageOrDep)]
--     extract blockNo entry = tryExtractPkg ++ tryExtractPrefs
--       where
--         tryExtractPkg = do
--           mkPkgEntry <- maybeToList $ extractPkg verbosity entry blockNo
--           return $ fmap (fmap Pkg) mkPkgEntry
--
--         tryExtractPrefs = do
--           prefs' <- maybeToList $ extractPrefs entry
--           fmap (return . Just . Dep) prefs'
--
-- -- | Turn the 'Entries' data structure from the @tar@ package into a list,
-- -- and pair each entry with its block number.
-- --
-- -- NOTE: This preserves the lazy nature of 'Entries': the tar file is only read
-- -- as far as the list is evaluated.
-- tarEntriesList :: Show e => Tar.Entries e -> [(BlockNo, Tar.Entry)]
-- tarEntriesList = go 0
--   where
--     go !_ Tar.Done = []
--     go !_ (Tar.Fail e) = error ("tarEntriesList: " ++ show e)
--     go !n (Tar.Next e es') = (n, e) : go (Tar.nextEntryOffset e n) es'
--
-- extractPkg :: Verbosity -> Tar.Entry -> BlockNo -> Maybe (IO (Maybe PackageEntry))
-- extractPkg verbosity entry blockNo = case Tar.entryContent entry of
--   Tar.NormalFile content _
--     | takeExtension fileName == ".cabal" ->
--         case splitDirectories (normalise fileName) of
--           [pkgname, vers, _] -> case simpleParsec vers of
--             Just ver -> Just . return $ Just (NormalPackage pkgid descr content blockNo)
--               where
--                 pkgid = PackageIdentifier (mkPackageName pkgname) ver
--                 parsed = parseGenericPackageDescriptionMaybe (BS.toStrict content)
--                 descr = case parsed of
--                   Just d -> d
--                   Nothing ->
--                     error $
--                       "Couldn't read cabal file "
--                         ++ show fileName
--             _ -> Nothing
--           _ -> Nothing
--   Tar.OtherEntryType typeCode content _
--     | Tar.isBuildTreeRefTypeCode typeCode ->
--         Just $ do
--           let path = byteStringToFilePath content
--           dirExists <- doesDirectoryExist path
--           if not dirExists
--             then return Nothing
--             else do
--               cabalFile <- tryFindAddSourcePackageDesc verbosity path "Error reading package index."
--               descr <- PackageDesc.Parse.readGenericPackageDescription normal cabalFile
--               return . Just $
--                 BuildTreeRef
--                   (refTypeFromTypeCode typeCode)
--                   (packageId descr)
--                   descr
--                   path
--                   blockNo
--   _ -> Nothing
--   where
--     fileName = Tar.entryPath entry
--
-- extractPrefs :: Tar.Entry -> Maybe [Dependency]
-- extractPrefs entry = case Tar.entryContent entry of
--   Tar.NormalFile content _
--     | isPreferredVersions entrypath ->
--         Just prefs
--     where
--       entrypath = Tar.entryPath entry
--       prefs = parsePreferredVersions content
--   _ -> Nothing
--
-- -- | Optimise sharing of equal values inside 'Cache'
-- --
-- -- c.f. https://en.wikipedia.org/wiki/Hash_consing
-- hashConsCache :: Cache -> Cache
-- hashConsCache cache0 =
--   cache0{cacheEntries = go mempty mempty (cacheEntries cache0)}
--   where
--     -- TODO/NOTE:
--     --
--     -- If/when we redo the binary serialisation via e.g. CBOR and we
--     -- are able to use incremental decoding, we may want to move the
--     -- hash-consing into the incremental deserialisation, or
--     -- alternatively even do something like
--     -- http://cbor.schmorp.de/value-sharing
--     --
--     go _ _ [] = []
--     -- for now we only optimise only CachePackageIds since those
--     -- represent the vast majority
--     go !pns !pvs (CachePackageId pid bno ts : rest) =
--       CachePackageId pid' bno ts : go pns' pvs' rest
--       where
--         !pid' = PackageIdentifier pn' pv'
--         (!pn', !pns') = mapIntern pn pns
--         (!pv', !pvs') = mapIntern pv pvs
--         PackageIdentifier pn pv = pid
--     go pns pvs (x : xs) = x : go pns pvs xs
--
--     mapIntern :: Ord k => k -> Map.Map k k -> (k, Map.Map k k)
--     mapIntern k m = maybe (k, Map.insert k k m) (\k' -> (k', m)) (Map.lookup k m)
