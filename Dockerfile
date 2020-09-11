FROM ubuntu:16.04

RUN apt-get update && \
    apt-get install -y redis-server && \
    apt-get clean

RUN apt-get install  -y apt
RUN apt-get install  -y gcc
RUN apt-get install  -y gfortran
RUN apt-get install  -y libopenmpi-dev
RUN apt-get install  -y python3
RUN apt-get install  -y gmsh
RUN apt-get install  -y git
RUN apt-get install  -y nano
RUN apt-get install  -y vim
RUN apt-get install  -y gnuplot
RUN apt-get install  -y zsh
RUN apt-get install  -y snap
RUN apt-get install  -y curl
RUN curl https://bootstrap.pypa.io/get-pip.py -o ~/get-pip.py
RUN python3 ~/get-pip.py
RUN pip install pillow
RUN pip install numpy
RUN pip install scipy
RUN pip install matplotlib
RUN pip install jupyter
RUN pip install nbzip
RUN pip install opencv-python
RUN git clone https://github.com/kazulagi/plantFEM.git
WORKDIR "/plantFEM"

RUN ls
RUN python3 install.py
RUN chmod +x bin/init
RUN chmod +x bin/update
RUN chmod +x bin/compress

CMD ["./plantfem","run"]