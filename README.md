# Merge Requests Stats Service
This project acts as a internal tool for a team using Gitlab merge requests for code review process. It uses Haskell Servant library for both getting things from Gitlab server and serving things for a frontend. 

Frontend part of the project can be found in [MR Stats Frontend in Elm](https://github.com/miciek/mr-stats-frontend-elm) repository.

## Running the project
To run you need to have both `GHC` and `cabal` in your path. There is also a `run.sh` file that sets up all needed environment variables. Please edit the `run.sh` file to run it against your own Gitlab server.

```
cabal sandbox init
cabal install --only-dependencies
cabal build
./run.sh
```

