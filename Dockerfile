FROM haskell:9.2.1-buster as cabal

# Update cabal to get the latest index.
RUN cabal update


FROM cabal as build-dependencies
WORKDIR /opt/project

# Copy the cabal files.
COPY cabal.project .
COPY ./core/*.cabal ./core/
COPY ./aeson/*.cabal ./aeson/

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

FROM cabal as doctest
WORKDIR /opt/project

RUN cabal install doctest

# Copy the cabal files.
COPY cabal.project .
COPY ./core/*.cabal ./core/
COPY ./aeson/*.cabal ./aeson/

# Install all the package dependencies
RUN cabal build --only-dependencies --enable-tests all

# Add and Install Application Code
COPY . .

CMD ["doctest", "core", "aeson"]
