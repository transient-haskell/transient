FROM agocorona/herokughcjs
RUN git clone https://github.com/agocorona/transient  transgit \
    && cd transgit \
    && git checkout ghcjs \
    && cabal install

RUN cd  transgit && cabal install --ghcjs

RUN git clone https://github.com/agocorona/ghcjs-perch \
    && cd ghcjs-perch \
    && cabal install \
    && cabal install --ghcjs #

RUN git clone https://github.com/agocorona/ghcjs-hplay  \
    && cd ghcjs-hplay \
    && cabal install \
    && cabal install --ghcjs
