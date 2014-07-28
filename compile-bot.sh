#!/bin/sh

cat lambda-ai/find_pill.scm lambda-ai/hand-driven.scm lambda-ai/prelude.scm | .cabal-sandbox/bin/LambdaCompiler > lambdabot.gcc
