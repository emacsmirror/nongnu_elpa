FROM python:3.13-slim
RUN mkdir /emacs-tests
WORKDIR /emacs-tests
RUN apt-get update
RUN apt-get install -y build-essential wget libgccjit-12-dev \
    zlib1g-dev libncurses-dev texinfo python3-venv
RUN python3 -m venv .venv
RUN wget http://mirror.cyberbits.eu/gnu/emacs/emacs-30.1.tar.xz
RUN tar -xvf emacs-30.1.tar.xz
WORKDIR ./emacs-30.1
RUN ./configure --prefix $(pwd)/.local --with-gnutls=ifavailable
RUN make
RUN make install
WORKDIR /emacs-tests
