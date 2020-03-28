#!/bin/bash
set -euxo pipefail

npm run clean

mkdir .tmp/

# Bundle the app
spago bundle-app --main Main --to .tmp/index.js

# Use the same index.html as in development
cp static/index.html .tmp/

# Parcel build
parcel build --public-url ./ .tmp/index.html

# Clean all
rm -r .tmp/
