FROM haskell:9.2.1-buster
WORKDIR /opt/project

# Update cabal to get the latest index.
RUN cabal update

# Install always used packages.
RUN cabal install containers mtl tasty tasty-hunit tasty-hedgehog hedgehog aeson aeson-pretty text bytestring scientific containers vector --constraint 'mtl >= 2.2.2 && tasty-hunit >= 0.10.0.3 && hedgehog >= 1.1.1'

# Copy the cabal files.
COPY cabal.project .
COPY ./core/*.cabal ./core/
COPY ./aeson/*.cabal ./aeson/

# Build all the dependencies
RUN cabal build --only-dependencies --enable-tests all

# Add and Install Application Code
COPY . .

# Build the actual code
RUN cabal build --enable-tests all

CMD ["cabal", "test", "--test-show-details=streaming", "all"]
