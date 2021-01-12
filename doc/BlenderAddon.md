# How to use Blender Add-on for SiCroF


## 0. Install SiCroF. 
```
(i) Click [Scripting] > [Open] 


(ii) Select "blSiCroFinstall.py" and click [Open text]


(iii) Click [Run script]
```
## 1. Create objects [Add] > [Sphere] or [Add] > [Cube] with names "Seed_*"

If one is named as "Seed_*", the obect is recognized as a seed object, which is deformable and permiable.

## 2. Set properties of the mesh object by [Object Properties] > [Cunstom properties]



```
(i) Click [Object properties] > [Custom Properties] > {Add}


(ii) Click [Edit] and edit [Property Name] and [Property Value]


Following [Property Name]s are available.

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

Boundary condition is set as a cube named "BC_*".\
You can add types and values of boundary conditions by [Object Properties] > [Cunstom properties]

## 4. Set properties of the boundary conditions.


```

(i) Click [Object properties] > [Custom Properties] > {Add}


(ii) Click [Edit] and edit [Property Name] and [Property Value]

Following [Property Name]s are available.

WaterContent
disp_x
disp_y
disp_z
```

## 5. Setup simulatior.

```
(i) Click [World properties] > [Custom Properties] > {Add}


(ii) Click [Edit] and edit [Property Name] and [Property Value]

(iii) Following [Property Name]s are available.

<True or False>
Display

<Float>
dt
nr_tol

<Integer>
interval
timestep

```



## 6. Export SiCroF script.

```
(i) Click [Scripting] > [Open] 


(ii) Select "blSiCroFexport.py" and click [Open text]


(iii) Click [Run script]
```

## 7. Run simulation.

```sequence
(i) Click [Scripting] > [Open] 


(ii) Select "blSiCroFrun.py" and click [Open text]


(iii) Click [Run script]
```