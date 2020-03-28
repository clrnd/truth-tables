#!/bin/bash
set -euxo pipefail

# Build the first time
spago build --no-install

# Leave a parcel watcher running
parcel static/index.html --open &

# And a spago one too
spago build --no-install --watch
