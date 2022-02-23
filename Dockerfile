FROM haskell:9.2.1-buster as install

WORKDIR /opt/project

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./*/*.cabal ./

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies all

FROM install as test

WORKDIR /opt/project

# Build test dependencies.
RUN cabal build --only-dependencies --enable-tests all

# Add and Install Application Code
COPY . .

# Build the actual code
RUN cabal build --enable-tests all

CMD ["cabal", "test", "--test-show-details=streaming", "all"]
