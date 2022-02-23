FROM haskell:9.2.1-buster as build-dependencies
WORKDIR /opt/project

# Update cabal to get the latest infex.
RUN cabal update

# Copy the cabal files.
COPY cabal.project .
COPY ./core/*.cabal ./core
COPY ./aeson/*.cabal ./aeson

# Install all the package dependencies
RUN cabal build --only-dependencies all

FROM build-dependencies as build-test-dependencies
WORKDIR /opt/project

# Build all the test dependencies
RUN cabal build --only-dependencies --enable-tests all
 
FROM build-test-dependencies as test
WORKDIR /opt/project

# Add and Install Application Code
COPY . .

# Build the actual code
RUN cabal build --enable-tests all

CMD ["cabal", "test", "--test-show-details=streaming", "all"]
