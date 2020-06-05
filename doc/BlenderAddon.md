# How to use Blender Add-on for SiCroF


## 1. Create objects [Add] > [Sphere] or [Add] > [Cube] with names "Seed_*"

If one is named as "Seed_*", the obect is recognized as a seed object, which is deformable and permiable.

## 2. Set properties of the mesh object by [Object Properties] > [Cunstom properties]

Following options are available.

```
YoungsModulus
PoissonRatio
Permiability
a_Psi
a_P
theta_eq
Psi_eq
a_E
a_v
E_eq
v_eq
```

## 3. Set Boundary Condition Object [Add] > [Cube]

## 4. Set properties of the boundary conditions.

Following options are available.


```
WaterContent
disp_x
disp_y
disp_z
```

## 5. Export script by [File] > [Export] > [Export SiCroF]


## 6. Go to the repository and run 

```
python3 SiCroF.py -s your_script_name.f90
```