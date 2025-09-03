#!/usr/bin/env bash

# Workflow
node scripts/semantic-release.js $1
npm run prettier:fix NEWS.md
R CMD build .
