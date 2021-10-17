#!/bin/sh

package_name=" "
mpif90="mpif90"

mkdir -p tmp

if [ "$1" = "install" ]; then
    package_name="$2"
    case "$package_name" in
        http*)
            curl -XGET $package_name --output "./tmp/${package_name##*/}"
            $mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path /opt/plantfem/inc/  -c "/opt/plantfem/tmp/${package_name##*/}" -o  /opt/plantfem/inc/"${package_name##*/}.o"
            echo /opt/plantfem/inc/"${package_name##*/}.o"
        ;;
        *)
            curl -XGET "https://plantfem.org/soja/$package_name.f90" -k --output "/opt/plantfem/tmp/$package_name.f90"
            $mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c "/opt/plantfem/tmp/$package_name.f90" -o  /opt/plantfem/inc/"$package_name.o"
            echo /opt/plantfem/inc/"$package_name.o"
        ;;
    esac
    
fi