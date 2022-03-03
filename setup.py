#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import setuptools
from skbuild import setup

requires=['numpy']

setup(
    name="skbuild-test",
    version='0.0.1',
    description='Performs line integrals through SAMI3 grids',
    author='John Haiducek',
    requires=requires,
    packages=['main']
)
#from skbuild import setup
#
#setup(name="plantfem", # パッケージの名前。pipでインストール後、pip list で表示される名前
#      version="22.4.0.0",
#      author="Haruka Tomobe",
#      author_email="my@name.com",
#      packages=["plantfem"], 
#      #cmake_install_dir="plantfem/bin", # 最初のディレクトリ名はpackagesで指定した名前と同じにする
#      entry_points={
#          "console_scripts": [
#              "plantfem=planttfem:plantfem"
#          ]
#          # cmdaaa がpip install後にシェルから使えるコマンド名になる
#          # =と:の間は、packagesで指定した名前と同じにする
#          # :の右側は後ほど説明
#      },
#      url="https://plantfem.org",
#      download_url="https://plantfem.org/download",
#      description="Sample", # このパッケージの短い説明文
#      long_description="Sample package !!!", # このパッケージの詳細な説明文
#      #classifiers=[
#      #    "Programming Language :: Fortran",
#      #    'Programming Language :: Python'
#      #],
#      include_package_data=True,
#      package_data={
#          "plantfem": ["inc/*o"],
#      },
#      license="MIT" # ライセンス名を記載(GPLとかApacheとか)
#)