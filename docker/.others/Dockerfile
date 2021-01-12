FROM jupyter/base-notebook

USER root
RUN apt-get update -y && apt-get install -y python-opencv

USER jovyan
RUN pip install https://github.com/ipython-contrib/jupyter_contrib_nbextensions/tarball/master \
jupyter_nbextensions_configurator pillow opencv-python
RUN jupyter contrib nbextension install --user && jupyter nbextensions_configurator enable --user