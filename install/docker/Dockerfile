FROM ubuntu:22.04

# Set user name and ID
#ARG USERNAME=knakashima
#ARG USER_UID=1000
#ARG USER_GID=$USER_UID

# Install necessary packages for plantFEM
RUN apt-get update && apt-get install -y \
    gcc \
    gfortran \
    libopenmpi-dev \  
    python3 \
    python3-pip \
    git \
    nano \
    gnuplot \
    make \
    sudo \
    liblapack-dev \
    libblas-dev \
    && rm -rf /var/lib/apt/lists/*

#RUN apt-get upgrade -y \
    #&& groupadd --gid $USER_GID $USERNAME \
    #&& useradd --uid $USER_UID --gid $USER_GID -m -s /bin/bash $USERNAME \
    #&& echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    #&& chmod 0440 /etc/sudoers.d/$USERNAME \
    #&& apt-get autoremove -y \
    #&& apt-get clean \
    #&& rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/kazulagi/plantfem.git
WORKDIR "/plantfem"

RUN python3 install.py
RUN chmod +x bin/init
RUN chmod +x bin/update
RUN chmod +x bin/compress
#COPY server.f90 /plantFEM/
CMD ["./plantfem","run"]

#WORKDIR /home/$USERNAME
#COPY . /home/$USERNAME

# [Optional] Set the default user. Omit if you want to keep the default as root.
#USER $USERNAME