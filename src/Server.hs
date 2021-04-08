module Server where

import DB
import qualified Database.Persist.Sqlite as P
import Hede
import Relude
import Snap.Core
import UnliftIO

routes :: MonadSnap m => m ()
routes = route
  [ ("dry", runDryHedeT routes'),
    ("real", runRealHedeT routes')
  ]

routes' :: (MonadSnap m, MonadHede m, MonadUnliftIO m) => m ()
routes' = route
  [ ("hoba", method GET hedeH),
    ("hobadb", method GET deeperHedeH)
  ]

hedeH :: (MonadSnap m, MonadHede m) => m ()
hedeH = do
  doHede "early"
  writeBS "hoba"

deeperHedeH :: (MonadSnap m, MonadHede m, MonadUnliftIO m) => m ()
deeperHedeH = do
  _ <- P.runSqlite ":memory:" $ do
    P.runMigration DB.migrateAll
    P.insert (Hede "hede1")

    doHede "done!"

    putStrLn "done some DB action"

  doHede "late"
  writeBS "hobadb"
