"""Minimal setup file for tasks project."""
import os
from setuptools import setup, find_packages

os.system("git clone https://github.com/kazulagi/plantFEM && cd plantFEM && python3 install.py")

setup(
    name='plantfem',
    version='21.10.1',
    license='MIT',
    description='Module Experiment',
    author='kazulagi',
    author_email='hogehoge@gmail.com',
    url='https://plantfem.org',
    packages=find_packages(where='src'),
    package_dir={'': 'src'},
)

print("Please add PATH by")
print("export PYTHONPATH=$PYTHONPATH:/opt/plantfem")