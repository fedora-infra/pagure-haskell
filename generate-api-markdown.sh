#!/usr/bin/env bash
cabal exec -- runhaskell -isrc generateDocs.hs  > API.md
