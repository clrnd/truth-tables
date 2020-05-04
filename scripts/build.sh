#!/bin/bash
set -euxo pipefail

npm run clean

TEMPDIR=app

mkdir ${TEMPDIR}

# Bundle the app
spago bundle-app --main Main --to ${TEMPDIR}/index.js

# Use the same index.html as in development
cp static/index.html ${TEMPDIR}/

# Parcel build
parcel build --public-url ./ ${TEMPDIR}/index.html

# Clean all
rm -r ${TEMPDIR}/
