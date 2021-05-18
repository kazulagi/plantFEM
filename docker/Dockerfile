FROM ubuntu:18.04

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

RUN apt-get install  -y gcc
RUN apt-get install  -y gfortran
RUN apt-get install  -y libopenmpi-dev
RUN apt-get install  -y python3
#RUN apt-get install  -y gmsh
RUN apt-get install  -y git
RUN apt-get install  -y nano
#RUN apt-get install  -y vim
RUN apt-get install  -y gnuplot
#RUN apt-get install  -y zsh
#RUN apt install -y jupyter-notebook
#RUN apt-get -yV install liblapack-dev
#RUN apt-get -yV install liblapack-doc
#RUN apt-get -yV install liblapack-pic
#RUN apt-get -yV install liblapack-test
#RUN apt-get -yV install liblapack3gf
#RUN apt -yV install curl
#RUN apt -yV install graphviz
#RUN curl https://bootstrap.pypa.io/get-pip.py -o ~/get-pip.py
#RUN python3 ~/get-pip.py
#RUN pip install pillow
#RUN pip install numpy
#RUN pip install scipy
#RUN pip install matplotlib
#RUN pip install jupyter
#RUN pip install nbzip
#RUN pip install opencv-python
#RUN pip install diagram
#RUN pip install diagrams

RUN git clone https://github.com/kazulagi/plantFEM.git
WORKDIR "/plantFEM"
RUN ls
RUN python3 install.py
RUN chmod +x bin/init
RUN chmod +x bin/update
RUN chmod +x bin/compress
COPY server.f90 .
CMD ["plantfem run"]