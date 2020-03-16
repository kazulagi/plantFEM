FROM ubuntu:18.04
ENV PYTHON_VERSION 3.7.5
USER root
ENV HOME /home

#ENV APP_PATH /home/SiCroF
#RUN sudo python3 install.py
#RUN python3 SiCroF.py
#WORKDIR $APP_PATH
#ENV APP_PATH /root/apps
#ENV PYTHON_ROOT $HOME/local/python-$PYTHON_VERSION
#ENV PATH $PYTHON_ROOT/bin:$PATH
#ENV PYENV_ROOT $HOME/.pyenv
#ENV DEBIAN_FRONTEND=noninteractive

# 環境構築
RUN apt-get update && apt-get upgrade -y \
    && apt-get install -y \
    git \
    gcc \
    gfortran \
    libopenmpi-dev \
    python3 \
    bash
#    make \
#    build-essential \
#    libssl-dev \
#    zlib1g-dev \
#    libbz2-dev \
#    libreadline-dev \
#    libsqlite3-dev \
#    wget \
#    curl \
#    llvm \
#    libncurses5-dev \
#    libncursesw5-dev \
#    xz-utils \
#    tk-dev \
#    libffi-dev \
#    liblzma-dev \
#    libmysqlclient-dev \
#    && git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT \
#    && $PYENV_ROOT/plugins/python-build/install.sh \
#    && /usr/local/bin/python-build -v $PYTHON_VERSION $PYTHON_ROOT \
#    && rm -rf $PYENV_ROOT
#RUN apt-get install -y libopenblas-base libomp-dev
#RUN pip install --no-cache-dir -r $APP_PATH/requirements.txt


#ホームディレクトリへ移動
WORKDIR $HOME
RUN /bin/bash -c pwd
RUN /bin/bash -c ls /

# コンテナ側ででSiCroFをダウンロード（非効率なんですがとりあえず手っ取り早そうなのでこうしてみました）
RUN git clone https://github.com/kazulagi/SiCroF.git
ENV APP_PATH /home/SiCroF
#RUN sudo python3 install.py
#RUN python3 SiCroF.py

#work directoryをSiCroFの中に移動
WORKDIR $APP_PATH
RUN /bin/bash -c pwd
RUN /bin/bash -c ls
RUN /bin/bash -c ls

# install.pyを実行しインストールしたいけど、ちゃんと動いてない。
RUN /bin/bash -c python3 install.py
RUN /bin/bash -c ls
RUN /bin/bash -c ls

# tutorialの中にあるサンプルコードを、$APP_PATHまでコピーしてきて動かしたい。
#RUN /bin/bash -c cp Tutorial/RouteOptimization/RouteOptimization.py test.py
#CMD ["/bin/sh", "python3", "Tutorial/RouteOptimization/RouteOptimization.py"]