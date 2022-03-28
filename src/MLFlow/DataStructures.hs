{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | MLFlow Bindings
module MLFlow.DataStructures where

--import Data.Maybe       (fromJust)
import Data.Aeson
import GHC.Generics

------------------------------------------------------------------------------
-- Data Structures
------------------------------------------------------------------------------

-- | Tags
data Tag = Tag { tagKey   :: String -- ^ The tag key.
               , tagValue :: String -- ^ The tag value.
               } deriving (Show)

instance FromJSON Tag where
  parseJSON (Object v) = do
      tagKey'   <- v .: "key"
      tagValue' <- v .: "value"
      pure (Tag tagKey' tagValue')
  parseJSON _ = fail "Expected an Object"

instance ToJSON Tag where
  toJSON Tag{..} = object [ "key"   .= tagKey
                          , "value" .=  tagValue ]

-- | Current Life Cycle Stage of the Experiment
data LifeCycleStage = Active 
                    | Deleted

instance Show LifeCycleStage where
  show Active  = "active"
  show Deleted = "deleted"
 
instance Read LifeCycleStage where
  readsPrec _ "active"  = [(Active, "")]
  readsPrec _ "deleted" = [(Deleted, "")]
  readsPrec _ _         = undefined

-- | Tag for an experiment.
type ExperimentTag = Tag

-- | Experiment
data Experiment = Experiment { -- | Unique identifier for the experiment.
                               experimentId     :: String
                               -- | Human readable name that identifies the experiment.
                             , experimentName   :: String
                               -- | Location where artifacts for the experiment are stored.
                             , artifactLocation :: FilePath
                               -- | Current life cycle stage of the experiment: “active” or “deleted”. 
                               -- Deleted experiments are not returned by APIs.
                             , lifecycleStage   :: LifeCycleStage
                               -- | Last update time
                             , lastUpdateTime   :: Maybe Int
                               -- | Creation time
                             , creationTime     :: Maybe Int
                               -- | Tags: Additional metadata key-value pairs.
                             , experimentTags   :: Maybe [ExperimentTag]
                             } deriving (Generic,Show)

instance FromJSON Experiment where
  parseJSON (Object v) = do
        experimentId'     <-                                    v .:  "experiment_id"
        experimentName'   <-                                    v .:  "name"
        artifactLocation' <-                                    v .:  "artifact_location"
        lifecycleStage'   <- (read::String->LifeCycleStage) <$> v .:  "lifecycle_stage"
        lastUpdateTime'   <-                                    v .:? "last_update_time"
        creationTime'     <-                                    v .:? "creation_time"
        experimentTags'   <-                                    v .:? "tags"
        pure (Experiment experimentId' experimentName' artifactLocation'
                         lifecycleStage' lastUpdateTime' creationTime' 
                         experimentTags')
  parseJSON _ = fail "Expected an Object"

instance ToJSON Experiment where
  toJSON Experiment{..} = object [ "experiment_id"     .= show experimentId
                                 , "name"              .= experimentName
                                 , "artifact_location" .= artifactLocation
                                 , "lifecycle_stage"   .= show lifecycleStage 
                                 , "last_update_time"  .= show lastUpdateTime
                                 , "creation_time"     .= show creationTime
                                 , "tags"              .= show experimentTags ]

-- | Metadata of a single artifact file or directory.
data FileInfo = FileInfo { path     :: FilePath -- ^ Path relative to the root artifact directory run.
                         , isDir    :: Bool     -- ^ Whether the path is a directory.
                         , fileSize :: Int      -- ^ Size in bytes. Unset for directories.
                         } deriving (Show)

instance FromJSON FileInfo where
  parseJSON (Object v) = do
      path'     <- v .: "path"
      isDir'    <- v .: "is_dir"
      fileSize' <- v .: "file_size"
      pure (FileInfo path' isDir' fileSize')
  parseJSON _ = fail "Expected an Object"

instance ToJSON FileInfo where
  toJSON FileInfo{..} = object [ "path"      .= path
                               , "is_dir"    .= isDir
                               , "file_size" .= fileSize ]

-- | Metric associated with a run, represented as a key-value pair.
data Metric = Metric { metricKey       :: String    -- ^ Key identifying this metric.
                     , metricValue     :: Float     -- ^  Value associated with this metric.
                     , metricTimeStamp :: Int       -- ^ The timestamp at which this metric was recorded.
                     , metricStep      :: Maybe Int -- ^ Step at which to log the metric.
                     } deriving (Show)

instance FromJSON Metric where
  parseJSON (Object v) = do
      metricKey'       <- v .:  "key"
      metricValue'     <- v .:  "value"
      metricTimeStamp' <- v .:  "timestamp"
      metricStep'      <- v .:? "step"
      pure (Metric metricKey' metricValue' metricTimeStamp' metricStep')
  parseJSON _ = fail "Expected an Object"

instance ToJSON Metric where
  toJSON Metric{..} = object [ "key"      .= metricKey
                             , "value"    .= metricValue
                             , "timestamp".= metricTimeStamp
                             , "step"     .= metricStep ]

data MetricLogger = MetricLogger { -- | ID of the run under which to log the
                                   -- metric. Must be provided.
                                   metricLoggerRunId     :: String
                                   -- | Name of the metric. This field is
                                   -- required.
                                 , metricLoggerKey       :: String
                                   -- | Double value of the metric being
                                   -- logged. This field is required.
                                 , metricLoggerValue     :: Float
                                   -- | Unix timestamp in milliseconds at the
                                   -- time metric was logged. This field is
                                   -- required.
                                 , metricLoggerTimeStamp :: Int
                                   -- | Step at which to log the metric.
                                 , metricLoggerStep      :: Maybe Int
                                 } deriving (Show)

instance FromJSON MetricLogger where
  parseJSON (Object v) = do
      metricRunId'     <- v .: "run_id"
      metricKey'       <- v .: "key"
      metricValue'     <- v .: "value"
      metricTimeStamp' <- v .: "timestamp"
      metricStep'      <- v .: "step"
      pure (MetricLogger metricRunId' metricKey' metricValue' metricTimeStamp' 
                         metricStep')
  parseJSON _ = fail "Expected an Object"

instance ToJSON MetricLogger where
  toJSON MetricLogger{..} = object [ "run_id"    .= metricLoggerRunId
                                   , "key"       .= metricLoggerKey
                                   , "value"     .= metricLoggerValue
                                   , "timestamp" .= metricLoggerTimeStamp
                                   , "step"      .= metricLoggerStep ]

-- | Batch Logging
data MetricBatchLogger = MetricBatchLogger { -- | ID of the run to log under
                                             mblRunId   :: String
                                             -- | Metrics to log. A single
                                             -- request can contain up to 1000
                                             -- metrics, and up to 1000
                                             -- metrics, params, and tags in
                                             -- total.
                                           , mblMetrics :: [Metric]
                                             -- | Params to log. A single
                                             -- request can contain up to 100
                                             -- params, and up to 1000 metrics,
                                             -- params, and tags in total.
                                           , mblParams  :: [Param]
                                             -- | Tags to log. A single request
                                             -- can contain up to 100 tags, and
                                             -- up to 1000 metrics, params, and
                                             -- tags in total.
                                           , mblTags    :: Maybe [Tag]
                                           } deriving (Show)

instance FromJSON MetricBatchLogger where
  parseJSON (Object v) = do
      runId'   <- v .:  "run_id"
      metrics' <- v .:  "metrics"
      params'  <- v .:  "params"
      tags'    <- v .:? "tags"
      pure (MetricBatchLogger runId' metrics' params' tags')
  parseJSON _ = fail "Expected an Object"

instance ToJSON MetricBatchLogger where
  toJSON MetricBatchLogger{..} = object [ "run_id"  .= mblRunId
                                        , "metrics" .= mblMetrics
                                        , "params"  .= mblParams
                                        , "tags"    .= mblTags]

-- | Tag for a model version.
type ModelVersionTag = Tag

-- | Status of a Model
data ModelVersionStatus = PendingRegistration -- ^ Request to register a new model version is pending as server performs background tasks.
                        | FailedRegistration  -- ^ Request to register a new model version has failed.
                        | Ready               -- ^ Model version is ready for use.

instance Show ModelVersionStatus where
  show PendingRegistration = "PENDING_REGISTRATION"
  show FailedRegistration  = "FAILED_REGISTRATION"
  show Ready               = "READY"
 
instance Read ModelVersionStatus where
  readsPrec _ "PENDING_REGISTRATION" = [(PendingRegistration, "")]
  readsPrec _ "FAILED_REGISTRATION"  = [(FailedRegistration, "")]
  readsPrec _ "READY"                = [(Ready, "")]
  readsPrec _  _                     = undefined

-- | Model Version
data ModelVersion = ModelVersion { -- | Unique name of the model
                                   modelName            :: String
                                   -- | Model’s version number.
                                 , modelVersion         :: String
                                   -- | Timestamp recorded when this
                                   -- model_version was created.
                                 , creationTimeStamp    :: Int
                                   -- | Timestamp recorded when metadata for
                                   -- this model_version was last updated.
                                 , lastUpdatedTimeStamp :: Int
                                   -- | User that created this model_version.
                                 , modelUserId          :: String
                                   -- | Current stage for this model_version.
                                 , currentStage         :: String
                                   -- | Description of this model_version.
                                 , modelDescription     :: String
                                   -- | URI indicating the location of the
                                   -- source model artifacts, used when
                                   -- creating model_version
                                 , modelSource          :: String
                                   -- | MLflow run ID used when creating
                                   -- model_version, if source was generated by
                                   -- an experiment run stored in MLflow
                                   -- tracking server.
                                 , modelRunId           :: String
                                   -- | Current status of model_version
                                 , modelStatus          :: ModelVersionStatus
                                   -- | Details on current status, if it is
                                   -- pending or failed.
                                 , modelStatusMessage   :: String
                                   -- | Tags: Additional metadata key-value
                                   -- pairs for this model_version.
                                 , modelTags            :: Maybe [ModelVersionTag]
                                   -- | Run Link: Direct link to the run that
                                   -- generated this version.
                                 , modelRunLink         :: String
                                 } deriving (Show)

instance FromJSON ModelVersion where
  parseJSON (Object v) = do
      modelName'            <-                                        v .:  "name"
      modelVersion'         <-                                        v .:  "version"
      creationTimeStamp'    <-                                        v .:  "creation_timestamp"
      lastUpdatedTimeStamp' <-                                        v .:  "last_updated_timestamp"
      modelUserId'          <-                                        v .:  "user_id"
      currentStage'         <-                                        v .:  "current_stage"
      modelDescription'     <-                                        v .:  "description"
      modelSource'          <-                                        v .:  "source"
      modelRunId'           <-                                        v .:  "run_id"
      modelStatus'          <- (read::String->ModelVersionStatus) <$> v .:  "status"
      modelStatusMessage'   <-                                        v .:  "status_message"
      modelTags'            <-                                        v .:? "tags"
      modelRunLink'         <-                                        v .:  "run_link"
      pure (ModelVersion modelName' modelVersion' creationTimeStamp'
                         lastUpdatedTimeStamp' modelUserId' currentStage'
                         modelDescription' modelSource' modelRunId'
                         modelStatus' modelStatusMessage' modelTags'
                         modelRunLink')
  parseJSON _ = fail "Expected an Object"

instance ToJSON ModelVersion where
  toJSON ModelVersion{..} = object [ "name"                  .= modelName
                                   , "version"               .= modelVersion
                                   , "creation_timestamp"    .= creationTimeStamp
                                   , "last_updated_timestamp".= lastUpdatedTimeStamp
                                   , "user_id"               .= modelUserId
                                   , "current_stage"         .= currentStage
                                   , "description"           .= modelDescription
                                   , "source"                .= modelSource
                                   , "run_id"                .= modelRunId
                                   , "status"                .= show modelStatus
                                   , "status_message"        .= modelStatusMessage
                                   , "tags"                  .= modelTags
                                   , "run_link"              .= modelRunLink ]

-- | Param associated with a run.
data Param = Param { paramKey   :: String -- ^ Key identifying this param.
                   , paramValue :: String -- ^ Value associated with this param.
                   } deriving (Show)

instance FromJSON Param where
  parseJSON (Object v) = do
      paramKey'   <- v .: "key"
      paramValue' <- v .: "value"
      pure (Param paramKey' paramValue')
  parseJSON _ = fail "Expected an Object"

instance ToJSON Param where
  toJSON Param{..} = object [ "key"   .= paramKey
                            , "value" .= paramValue  ]

-- | Tag for a registered model
type RegisteredModelTag = Tag

data RegisteredModel = RegisteredModel { -- | Unique name for the model.
                                         regName                 :: String 
                                         -- | Timestamp recorded when this
                                         -- registered_model was created.
                                       , regCreationTimeStamp    :: Int
                                         -- | Timestamp recorded when metadata
                                         -- for this registered_model was last
                                         -- updated.
                                       , regLastUpdatedTimeStamp :: Int
                                         -- | User that created this registered_model
                                       , regUserId               :: String
                                         -- | Description of this registered_model.
                                       , regDescription          :: String
                                         -- | Collection of latest model
                                         -- versions for each stage. Only
                                         -- contains models with current READY
                                         -- status.
                                       , latestVersions          :: [ModelVersion]
                                         -- | Tags: Additional metadata
                                         -- key-value pairs for this
                                         -- registered_model.
                                       , regTags                 :: Maybe [RegisteredModelTag]
                                       } deriving (Show)

instance FromJSON RegisteredModel where
  parseJSON (Object v) = do
      regName'                 <- v .:  "name"
      regCreationTimeStamp'    <- v .:  "creation_timestamp"
      regLastUpdatedTimeStamp' <- v .:  "last_updated_timestamp"
      regUserId'               <- v .:  "user_id"
      regDescription'          <- v .:  "description"
      latestVersions'          <- v .:  "latest_versions"
      regTags'                 <- v .:? "tags"
      pure (RegisteredModel regName' regCreationTimeStamp' regLastUpdatedTimeStamp' 
                            regUserId' regDescription' latestVersions' regTags' )
  parseJSON _ = fail "Expected an Object"

instance ToJSON RegisteredModel where
  toJSON RegisteredModel{..} = object [ "name"                   .= regName
                                      , "creation_timestamp"     .= regCreationTimeStamp
                                      , "last_updated_timestamp" .= regLastUpdatedTimeStamp
                                      , "user_id"                .= regUserId
                                      , "description"            .= regDescription
                                      , "latest_versions"        .= latestVersions
                                      , "tags"                   .= regTags ]

-- | Tag for a run.
type RunTag = Tag

-- | Status of a run.
data RunStatus = Running   -- ^ Run has been initiated.
               | Scheduled -- ^ Run is scheduled to run at a later time.
               | Finished  -- ^ Run has completed.
               | Failed    -- ^ Run execution failed.
               | Killed    -- ^ Run killed by user.
  deriving (Eq)

instance Show RunStatus where
  show Running   = "RUNNING"
  show Scheduled = "SCHEDULED"
  show Finished  = "FINISHED"
  show Failed    = "FAILED"
  show Killed    = "KILLED"

instance Read RunStatus where
  readsPrec _ "RUNNING"   = [(Running, "")]
  readsPrec _ "SCHEDULED" = [(Scheduled, "")]
  readsPrec _ "FINISHED"  = [(Finished, "")]
  readsPrec _ "FAILED"    = [(Failed, "")]
  readsPrec _ "KILLED"    = [(Killed, "")]
  readsPrec _ _           = undefined

-- | Run data (metrics, params, and tags).
data RunData = RunData { runMetrics :: Maybe [Metric] -- ^ Run metrics.
                       , runParams  :: Maybe [Param]  -- ^ Run parameters.
                       , runTags    :: Maybe [RunTag] -- ^ Additional metadata key-value pairs.
                       } deriving (Show)

instance FromJSON RunData where
  parseJSON (Object v) = do
      runMetrics' <- v .:? "metrics"
      runParams'  <- v .:? "params"
      runTags'    <- v .:? "tags"
      pure (RunData runMetrics' runParams' runTags')
  parseJSON _ = fail "Expected an Object"

instance ToJSON RunData where
  toJSON RunData{..} = object [ "metrics" .= runMetrics
                              , "params"  .= runParams
                              , "tags"    .= runTags ]

-- | Metadata of a single run.
data RunInfo = RunInfo { -- | Unique identifier for the run.
                         runId             :: String
                         -- | The experiment ID.
                       , runExperimentId   :: String
                         -- | Current status of the run.
                       , runStatus         :: RunStatus
                         -- | Unix timestamp of when the run started in milliseconds.
                       , runStartTime      :: Int
                         -- | Unix timestamp of when the run ended in milliseconds.
                       , runEndTime        :: Maybe Int
                         -- | URI of the directory where artifacts should be
                         -- uploaded. This can be a local path (starting with
                         -- “/”), or a distributed file system (DFS) path, like
                         -- s3://bucket/directory or dbfs:/my/directory. If not
                         -- set, the local ./mlruns directory is chosen.
                       , runArtifactUri    :: String
                         -- | Current life cycle stage of the experiment :
                         -- OneOf(“active”, “deleted”)
                       , runLifeCycleStage :: LifeCycleStage
                       } deriving (Show)

instance FromJSON RunInfo where
  parseJSON (Object v) = do
      runId'             <-                                    v .:  "run_id"
      runExperimentId'   <-                                    v .:  "experiment_id"
      runStatus'         <-      (read::String->RunStatus) <$> v .:  "status"
      runStartTime'      <-                                    v .:  "start_time"
      runEndTime'        <-                                    v .:? "end_time"
      runArtifactUri'    <-                                    v .:  "artifact_uri"
      runLifeCycleStage' <- (read::String->LifeCycleStage) <$> v .:  "lifecycle_stage"
      pure (RunInfo runId' runExperimentId' runStatus' runStartTime' runEndTime' 
                    runArtifactUri' runLifeCycleStage')
  parseJSON _ = fail "Expected an Object"

instance ToJSON RunInfo where
  toJSON RunInfo{..} = object [ "run_id"          .= runId
                              , "experiment_id"   .= show runExperimentId
                              , "status"          .= show runStatus
                              , "start_time"      .= runStartTime
                              , "end_time"        .= runEndTime
                              , "artifact_uri"    .= runArtifactUri
                              , "lifecycle_stage" .= show runLifeCycleStage ]

-- | A single run.
data Run = Run { runInfo :: RunInfo -- ^ Run metadata.
               , runData :: RunData -- ^ Run data.
               } deriving (Show)

instance FromJSON Run where
  parseJSON (Object v) = do
      runInfo' <- v .: "info"
      runData' <- v .: "data"
      pure (Run runInfo' runData')
  parseJSON _ = fail "Expected an Object"

instance ToJSON Run where
  toJSON Run{..} = object [ "info" .= runInfo
                          , "data" .= runData ]

-- | Request Structure for Creating Runs
data RunCreator = RunCreator { -- | ID of the associated experiment.
                               runExpId'     :: String
                               -- | Unix timestamp in milliseconds of when the
                               -- run started.
                             , runStartTime' :: Int
                               -- | Additional metadata for run.
                             , runTags'      :: Maybe [RunTag]
                             } deriving (Show)

instance FromJSON RunCreator where
  parseJSON (Object v) = do
      expId'     <- v .:  "experiment_id"
      startTime' <- v .:  "start_time"
      tags'      <- v .:? "tags"
      pure (RunCreator expId' startTime' tags')
  parseJSON _ = fail "Expected an Object"

instance ToJSON RunCreator where
  toJSON RunCreator{..} = object [ "experiment_id" .= runExpId'
                                 , "start_time"    .= runStartTime'
                                 , "tags"          .= runTags' ]

-- | Request Structure for Updating Run metadata.
data RunUpdater = RunUpdater { uRunId   :: String    -- ^ ID of the run to update. Must be provided.
                             , uStatus  :: RunStatus -- ^ Updated status of the run.
                             , uEndTime :: Maybe Int -- ^ Unix timestamp in milliseconds of when the run ended.
                             } deriving (Show)

instance FromJSON RunUpdater where
  parseJSON (Object v) = do
      runId'   <-                               v .:  "run_id"
      status'  <- (read::String->RunStatus) <$> v .:  "status"
      endTime' <-                               v .:? "end_time"
      pure (RunUpdater runId' status' endTime')
  parseJSON _ = fail "Expected an Object"

instance ToJSON RunUpdater where
  toJSON RunUpdater{..} = object [ "runN_id"  .= uRunId
                                 , "status"   .= show uStatus
                                 , "end_time" .= uEndTime  ]

-- | View type for ListExperiments query.
data ViewType = ActiveOnly  -- ^ Default. Return only active experiments.
              | DeletedOnly -- ^ Return only deleted experiments.
              | All         -- ^ Get all experiments.

instance Show ViewType where
  show ActiveOnly  = "ACTIVE_ONLY"
  show DeletedOnly = "DELETED_ONLY"
  show All         = "ALL"

instance Read ViewType where
  readsPrec _ "ACTIVE_ONLY"  = [(ActiveOnly, "")]
  readsPrec _ "DELETED_ONLY" = [(DeletedOnly, "")]
  readsPrec _ "ALL"          = [(All, "")]
  readsPrec _ _              = undefined
