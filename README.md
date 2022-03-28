# mlflow-hs

Rudimentary and mostly incomplete bindings for logging to an
[mlflow](https://www.mlflow.org) tracking server from Haskell.

## Basic Usage

Launch tracking server:

```bash
$ mlflow server -h 0.0.0.0 -p 5000
```

Import the `MLFlow` module:

```haskell
import MLFlow
```

Set Tracking URI:

```haskell
uri = trackingURI' "localhost" 5000
```

Create Experiment:

```haskell
experimentId <- createExperiment uri "experiment_name"
```

Create Run:

```haskell
runId' <- runId . runInfo <$> createRun baseUrl experimentId []
```

Log Metric:

```haskell
res <- logMetric uri runId' "some_metric" 6.66 0
```
