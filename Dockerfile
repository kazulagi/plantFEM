FROM ubuntu:18.04
MAINTAINER Marco Pantaleoni <marco.pantaleoni@gmail.com>

RUN echo "Europe/Rome" > /etc/timezone
# RUN sudo ln -fs /usr/share/zoneinfo/Europe/Rome /etc/localtime

RUN apt-get update -q && \
	export DEBIAN_FRONTEND=noninteractive && \
    apt-get install -y --no-install-recommends tzdata

RUN dpkg-reconfigure -f noninteractive tzdata

# Install packages
RUN apt-get update -q && \
	export DEBIAN_FRONTEND=noninteractive && \
    apt-get install -y --no-install-recommends wget curl rsync netcat mg vim bzip2 zip unzip && \
    apt-get install -y --no-install-recommends libx11-6 libxcb1 libxau6 && \
    apt-get install -y --no-install-recommends lxde tightvncserver xvfb dbus-x11 x11-utils && \
    apt-get install -y --no-install-recommends xfonts-base xfonts-75dpi xfonts-100dpi && \
    apt-get install -y --no-install-recommends python-pip python-dev python-qt4 && \
    apt-get install -y --no-install-recommends libssl-dev && \
    apt-get autoclean -y && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    wget \
    git \
    libssl-dev \
    libbz2-dev \
    libsqlite3-dev \
    libreadline-dev \
    zlib1g-dev \
    libasound2-dev \
    libxss1 \
    libxtst6 \
    gdebi

RUN wget -O vscode-amd64.deb https://go.microsoft.com/fwlink/?LinkID=760868
RUN yes | gdebi vscode-amd64.deb
RUN rm vscode-amd64.deb

WORKDIR /root/

RUN apt-get upgrade -y
RUN apt update -y
RUN apt upgrade -y
RUN mkdir -p /root/.vnc
COPY docker/xstartup /root/.vnc/
RUN chmod a+x /root/.vnc/xstartup
RUN touch /root/.vnc/passwd
RUN /bin/bash -c "echo -e 'password\npassword\nn' | vncpasswd" > /root/.vnc/passwd
RUN chmod 400 /root/.vnc/passwd
RUN chmod go-rwx /root/.vnc
RUN touch /root/.Xauthority

COPY docker/start-vncserver.sh /root/
RUN chmod a+x /root/start-vncserver.sh


RUN echo "mycontainer" > /etc/hostname
RUN echo "127.0.0.1	localhost" > /etc/hosts
RUN echo "127.0.0.1	mycontainer" >> /etc/hosts


#RUN chmod +x bin/init
#RUN chmod +x bin/update
#RUN chmod +x bin/compress


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
RUN apt-get install  -y snapd
RUN apt-get install -y blender
RUN apt-get install -y curl
RUN curl -L https://go.microsoft.com/fwlink/?LinkID=760868 -o vscode.deb
RUN apt-get install -y libsecret-1-0
RUN dpkg -i vscode.deb

RUN git clone https://github.com/kazulagi/plantFEM.git
WORKDIR "/plantFEM"

EXPOSE 5901
ENV USER root
CMD [ "/root/start-vncserver.sh" ]
#FROM ubuntu:18.04



#FROM ubuntu:16.04
#
#ENV DEBIAN_FRONTEND noninteractive
#ENV USER root
#
#RUN apt-get update && \
#    apt-get install -y --no-install-recommends ubuntu-desktop && \
#    apt-get install -y gnome-panel gnome-settings-daemon metacity nautilus gnome-terminal && \
#    apt-get install -y tightvncserver && \
#    mkdir -p /root && \
#    mkdir -p /root/.vnc
#
#ADD xstartup /root/.vnc/xstartup
#ADD passwd /root/.vnc/passwd
#
#RUN chmod 600 /root/.vnc/passwd
#
#RUN apt-get update && \
#    apt-get install -y redis-server && \
#    apt-get clean
#
#RUN apt install  -y gcc
#RUN apt install  -y gfortran
#RUN apt install  -y libopenmpi-dev
#RUN apt install  -y python3
#RUN apt install  -y gmsh
#RUN apt install  -y git
#RUN apt install  -y nano
#RUN apt install  -y vim
#RUN apt install  -y gnuplot
#RUN apt install  -y zsh
#RUN apt install  -y snap
#RUN apt install  -y snapd
#RUN snap install  -y blender --classic
#RUN snap install  -y code --classic



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

#RUN git clone https://github.com/kazulagi/plantFEM.git
#WORKDIR "/plantFEM"
#RUN ls
#RUN python3 install.py
#RUN chmod +x bin/init
#RUN chmod +x bin/update
#RUN chmod +x bin/compress
#
#
#CMD /usr/bin/vncserver :1 -geometry 1280x800 -depth 24 && tail -f /root/.vnc/*:1.log
#
#EXPOSE 5901
#