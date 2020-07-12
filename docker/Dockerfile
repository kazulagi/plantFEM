FROM ubuntu:18.04

RUN apt-get update && \
    apt-get install -y redis-server && \
    apt-get clean

RUN apt install  -y gcc
RUN apt install  -y gfortran
RUN apt install  -y libopenmpi-dev
RUN apt install  -y python3
RUN apt install  -y gmsh
RUN apt install  -y git
RUN apt install  -y nano
RUN apt install  -y vim
RUN apt install  -y gnuplot
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
RUN python3 install.py

CMD ["./plantfem ./home/untitled/main.f90"]