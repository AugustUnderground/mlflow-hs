{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell Bindings for MLFlow: https://www.mlflow.org/
module MLFlow where

import MLFlow.DataStructures

-- import Data.Char             (isLower)
-- import Data.List             (isInfixOf, isPrefixOf, isSuffixOf, elemIndex)
import Data.Maybe            (fromJust)
import Data.Aeson
import Network.Wreq
import Control.Lens
-- import Control.Monad
-- import GHC.Generics
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import System.Directory
import qualified Data.Map              as M
import qualified Data.ByteString.Lazy  as BL
-- import qualified Data.ByteString       as BS hiding (pack)
-- import qualified Data.ByteString.Char8 as BS        (pack)

------------------------------------------------------------------------------
-- Tracking
------------------------------------------------------------------------------

type TrackingURI  = String -- ^ Base URL and Port to mlflow server

-- | Hostname and port to TrackingURI
trackingURI :: String -> String -> TrackingURI
trackingURI host port = "http://" ++ host ++ ":" ++ port

-- | Hostname and port (as Int) to TrackingURI
trackingURI' :: String -> Int -> TrackingURI
trackingURI' host port = "http://" ++ host ++ ":" ++ show port

------------------------------------------------------------------------------
-- Experiments
------------------------------------------------------------------------------

type ExperimentID = String -- ^ Experiment ID

-- |  Create an experiment with a name. Returns the ID of the newly created
-- experiment. Validates that another experiment with the same name does not
-- already exist and fails if another experiment with the same name already
-- exists.
createExperiment :: TrackingURI -> String -> IO ExperimentID
createExperiment baseUrl name' = do
    res <- BL.toStrict . (^. responseBody) <$> post url payload
    let expId = fromJust $ M.lookup "experiment_id" 
                    ((fromJust $ decodeStrict res) :: M.Map String String)
    pure expId
  where
    url  = baseUrl ++ "/api/2.0/mlflow/experiments/create"
    payload = toJSON (M.fromList [("name", name')] :: M.Map String String)

-- | Get a list of all experiments.
listExperiments :: TrackingURI -> IO [Experiment]
listExperiments baseUrl = do
    res <- BL.toStrict . (^. responseBody) <$> get url
    let experiments = fromJust $ M.lookup "experiments" 
                        (fromJust $ decodeStrict res :: M.Map String [Experiment])
    pure experiments
  where
    url  = baseUrl ++ "/api/2.0/mlflow/experiments/list"

-- | Get metadata for an experiment. This method works on deleted experiments.
getExperiment :: TrackingURI -> ExperimentID -> IO Experiment
getExperiment baseUrl expId = do
    res <- BL.toStrict . (^. responseBody) <$> get url
    let expm = fromJust . M.lookup "experiment" 
             $ (fromJust $ decodeStrict res :: M.Map String Experiment)
    pure expm
  where
    url  = baseUrl ++ "/api/2.0/mlflow/experiments/get?experiment_id=" ++ expId

-- | Get metadata for an experiment. (By Name)
getExperimentByName :: TrackingURI -> String -> IO Experiment
getExperimentByName baseUrl expName = do
    res <- BL.toStrict . (^. responseBody) <$> get url
    let expm = fromJust . M.lookup "experiment" 
             $ (fromJust $ decodeStrict res :: M.Map String Experiment)
    pure expm
  where
    url  = baseUrl ++ "/api/2.0/mlflow/experiments/get-by-name?experiment_name=" 
                   ++ expName

-- | Mark an experiment and associated metadata, runs, metrics, params, and
-- tags for deletion. If the experiment uses FileStore, artifacts associated
-- with experiment are also deleted.
deleteExperiment :: TrackingURI -> ExperimentID -> IO (Response BL.ByteString)
deleteExperiment baseUrl expId = post url payload
  where
    url = baseUrl ++ "/api/2.0/mlflow/experiments/delete" 
    payload = toJSON (M.fromList [("experiment_id", expId)] :: M.Map String String)

-- | Restore an experiment marked for deletion. This also restores associated
-- metadata, runs, metrics, params, and tags. If experiment uses FileStore,
-- underlying artifacts associated with experiment are also restored.
resoterExperiment :: TrackingURI -> ExperimentID -> IO (Response BL.ByteString)
resoterExperiment baseUrl expId = post url payload
  where
    url = baseUrl ++ "/api/2.0/mlflow/experiments/restore" 
    payload = toJSON (M.fromList [("experiment_id", expId)] :: M.Map String String)

-- | Update experiment metadata.
updateExperiment :: TrackingURI -> ExperimentID -> String -> IO (Response BL.ByteString)
updateExperiment baseUrl expId newName = post url payload
  where
    url = baseUrl ++ "/api/2.0/mlflow/experiments/update" 
    payload = toJSON (M.fromList [ ("experiment_id", expId)
                                 , ("new_name", newName)
                                 ] :: M.Map String String)

------------------------------------------------------------------------------
-- Runs
------------------------------------------------------------------------------

type RunID = String -- ^ Run ID

-- | Create a new run within an experiment. A run is usually a single execution
-- of a machine learning or data ETL pipeline. MLflow uses runs to track Param,
-- Metric, and RunTag associated with a single execution.
createRun :: TrackingURI -> ExperimentID -> [Tag] -> IO Run
createRun baseUrl expId' tags' = do
    startTime <- (round . (* 1000) <$> getPOSIXTime :: IO Int)
    let payload = toJSON (RunCreator expId' startTime (Just tags'))
    res <- BL.toStrict . (^. responseBody) <$> post url payload
    let run' = fromJust $ decodeStrict res :: M.Map String Run
        run  = fromJust . M.lookup "run" $ run'
    pure run
  where
    url = baseUrl ++ "/api/2.0/mlflow/runs/create" 

-- | Mark a run for deletion.
deleteRun :: TrackingURI -> RunID -> IO (Response BL.ByteString)
deleteRun baseUrl runId' = post url payload
  where
    url     = baseUrl ++ "/api/2.0/mlflow/runs/delete" 
    payload = toJSON (M.fromList [("run_id", runId')] :: M.Map String String)

-- | Restore a deleted run.
restoreRun :: TrackingURI -> RunID -> IO (Response BL.ByteString)
restoreRun baseUrl runId' = post url payload
  where
    url     = baseUrl ++ "/api/2.0/mlflow/runs/restore" 
    payload = toJSON (M.fromList [("run_id", runId')] :: M.Map String String)

-- | Get metadata, metrics, params, and tags for a run. In the case where
-- multiple metrics with the same key are logged for a run, return only the
-- value with the latest timestamp. If there are multiple values with the
-- latest timestamp, return the maximum of these values.
getRun :: TrackingURI -> RunID -> IO Run
getRun baseUrl runId' = do
    res <- BL.toStrict . (^. responseBody) <$> get url
    let run = fromJust . M.lookup "run" 
            $ (fromJust $ decodeStrict res :: M.Map String Run)
    pure run
  where
    url = baseUrl ++ "/api/2.0/mlflow/runs/get?run_id=" ++ runId'

-- | Update run metadata.
updateRun :: TrackingURI -> RunID -> RunStatus -> Maybe Int -> IO RunInfo
updateRun baseUrl runId' status' endTime' = do
    res <- BL.toStrict . (^. responseBody) <$> post url payload
    let runInfo = fromJust . M.lookup "run_info" 
                $ (fromJust $ decodeStrict res :: M.Map String RunInfo)
    pure runInfo
  where
    url     = baseUrl ++ "/api/2.0/mlflow/runs/update"
    payload = toJSON (RunUpdater runId' status' endTime')

------------------------------------------------------------------------------
-- Logging
------------------------------------------------------------------------------

-- | Log a metric for a run. A metric is a key-value pair (string key, float
-- value) with an associated timestamp. Examples include the various metrics
-- that represent ML model accuracy. A metric can be logged multiple times.
logMetric :: TrackingURI -> RunID -> String -> Float -> Int 
          -> IO (Response BL.ByteString)
logMetric baseUrl runId' key value step = do
    timeStamp <- (round . (* 1000) <$> getPOSIXTime :: IO Int)
    let payload = toJSON (MetricLogger runId' key value timeStamp (Just step))
    post url payload
  where
    url  = baseUrl ++ "/api/2.0/mlflow/runs/log-metric"

-- | Log a batch of metrics, params, and tags for a run. If any data failed to
-- be persisted, the server will respond with an error (non-200 status code).
-- In case of error (due to internal server error or an invalid request),
-- partial data may be written.
logBatch :: TrackingURI -> RunID -> [Metric] -> [Param] -> [Tag] 
         -> IO (Response BL.ByteString)
logBatch baseUrl runId' metrics' params' tags' = post url payload
  where
    payload = toJSON (MetricBatchLogger runId' metrics' params' (Just tags'))
    url  = baseUrl ++ "/api/2.0/mlflow/runs/log-batch"

-- | Just for Convenience
logBatch' :: TrackingURI -> RunID -> Int -> M.Map String Float 
          -> M.Map String String -> IO (Response BL.ByteString)
logBatch' baseUrl runId' step metrics parameters = do
    timeStamp <- (round . (* 1000) <$> getPOSIXTime :: IO Int)
    let metrics' = M.elems $ M.mapWithKey (\key value -> Metric key value timeStamp (Just step)) metrics
        params'  = M.elems $ M.mapWithKey Param parameters
    logBatch baseUrl runId' metrics' params' []

-- | Log a param used for a run. A param is a key-value pair (string key,
-- string value). Examples include hyperparameters used for ML model training
-- and constant dates and values used in an ETL pipeline. A param can be logged
-- only once for a run.
logParam :: TrackingURI -> RunID -> String -> String -> IO (Response BL.ByteString)
logParam baseUrl runId' key value = post url payload
  where
    payload = toJSON (M.fromList [ ("run_id", runId')
                                 , ("key", key)
                                 , ("value", value)
                                 ] :: M.Map String String)
    url  = baseUrl ++ "/api/2.0/mlflow/run/log-parameter"

-- | Get a list of all values for the specified metric for a given run.
getMetricHistory :: TrackingURI -> RunID -> String -> IO [Metric]
getMetricHistory baseUrl runId' metricKey = do
    res <- BL.toStrict . (^. responseBody) <$> get url
    let metrics' = fromJust . M.lookup "metrics" 
                 $ (fromJust $ decodeStrict res :: M.Map String [Metric])
    pure metrics'
  where
    url  = baseUrl ++ "/api/2.0/mlflow/metrics/get-history?run_id=" ++ runId'
                   ++ "?metric_key=" ++ metricKey

------------------------------------------------------------------------------
-- Tagging
------------------------------------------------------------------------------

-- | Set a tag on an experiment. Experiment tags are metadata that can be
-- updated.
setExperimentTag :: TrackingURI -> ExperimentID -> String -> String 
                 -> IO (Response BL.ByteString)
setExperimentTag baseUrl expId' key value = post url payload
  where
    payload = toJSON (M.fromList [ ("experiment_id", expId')
                                 , ("key", key)
                                 , ("value", value)
                                 ] :: M.Map String String)
    url  = baseUrl ++ "/api/2.0/mlflow/experiments/set-experiment-tag"

-- | Set a tag on a run. Tags are run metadata that can be updated during a run
-- and after a run completes.
setRunTag :: TrackingURI -> RunID -> String -> String 
          -> IO (Response BL.ByteString)
setRunTag baseUrl runId' key value = post url payload
  where
    payload = toJSON (M.fromList [ ("run_id", runId')
                                 , ("key", key)
                                 , ("value", value)
                                 ] :: M.Map String String)
    url  = baseUrl ++ "/api/2.0/mlflow/runs/set-tag"

-- | Delete a tag on a run. Tags are run metadata that can be updated during a
-- run and after a run completes.
deleteTag :: TrackingURI -> RunID -> String -> IO (Response BL.ByteString)
deleteTag baseUrl runId' key = post url payload
  where
    payload = toJSON (M.fromList [ ("run_id", runId')
                                 , ("key", key)
                                 ] :: M.Map String String)
    url  = baseUrl ++ "/api/2.0/mlflow/runs/delete-tag"




--{ metricKey       :: String    -- ^ Key identifying this metric.
--                     , metricValue     :: Float     -- ^  Value associated with this metric.
--                     , metricTimeStamp :: Int       -- ^ The timestamp at which this metric was recorded.
--                     , metricStep      :: Maybe Int -- ^ Step at which to log the metric.
--                     }

-- payload = toJSON $ M.fromList [("name", "foobar666")]
-- res <- BL.toStrict . (^. responseBody) <$> post "http://134.103.69.10:5000/api/2.0/mlflow/experiments/create" payload
-- 
--payload = toJSON $ M.fromList [("experiment_id", "5"), ("name", "foobar666")]
--res <- BL.toStrict . (^. responseBody) <$> post "http://134.103.69.10:5000/api/2.0/mlflow/runs/create" payload
-- 
-- fromJust $ decodeStrict res :: M.Map String [Experiment]
-- 
-- fromJust $ M.fromList . map (\m -> ((read::String->Int) . fromJust $ M.lookup "experiment_id" m, fromJust $ M.lookup "name" m)) <$> M.lookup "experiments" bar
