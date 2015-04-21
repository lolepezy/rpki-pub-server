FROM haskell:7.8

RUN cabal update

# Add .cabal file
ADD ../rpki-pub-server.cabal /opt/rpki-pub-server/rpki-pub-server.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt/rpki-pub-server && cabal install --only-dependencies -j4

# Add and Install Application Code
ADD ../src /opt/rpki-pub-server/src
ADD ../repo /opt/rpki-pub-server/repo
RUN cd /opt/rpki-pub-server/ && cabal install

# Add installed cabal executables to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt/rpki-pub-server/
CMD ["rpki-pub-server --repo-path=/opt/rpki-pub-server/repo --session=9df4b597-af9e-4dca-bdda-719cce2c4e28"]
