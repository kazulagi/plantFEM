FROM ubuntu:18.04

ENV PYTHON_VERSION 3.7.5
ENV HOME /home
ENV APP_PATH /home/SiCroF
ENV PYTHON_ROOT $HOME/local/python-$PYTHON_VERSION
ENV PATH $PYTHON_ROOT/bin:$PATH
ENV PYENV_ROOT $HOME/.pyenv
ENV DEBIAN_FRONTEND=noninteractive

# 環境構築
RUN apt update && apt upgrade -y \
    && apt install -y \
    build-essential \
    gfortran \
    libopenmpi-dev \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    git \
    wget \
    && git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT \
    && $PYENV_ROOT/plugins/python-build/install.sh \
    && /usr/local/bin/python-build -v $PYTHON_VERSION $PYTHON_ROOT \
    && git clone https://github.com/kazulagi/SiCroF.git $APP_PATH \
    && python $APP_PATH/setup.py \
    && python $APP_PATH/install.py

WORKDIR $APP_PATH
