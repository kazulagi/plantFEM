#!/bin/sh

package_name=" "
mpif90="mpif90"

if [ "$1" = "install" ]; then
    package_name="$2"
    curl -XGET "https://plantfem.org/soja/$package_name.f90" -k --output "./tmp/$package_name.f90"
    $mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c "./tmp/$package_name.f90" -o  ./inc/"$package_name.o"
    echo ./inc/"$package_name.o"
fi