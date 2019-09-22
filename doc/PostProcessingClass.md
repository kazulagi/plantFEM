## Class Name :: PostProcessingClass

[README](README.md)>>[PostProcessingClass](Document/PostProcessingClass.md)

### Instruction:
PostProcessingClass is for post processing for results of Finite Element Method. Following methods are implemented.

* GmshPlotContour
```
subroutine GmshPlotContour(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep)
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6

```



* GmshPlotContour2D
```
subroutine GmshPlotContour2D(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep)
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6

```



* GmshExportStress
```
subroutine GmshExportStress(obj,uvec,sigma,strain_measure,step )
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::uvec(:),sigma(:,:,:),strain_measure(:,:,:)
	integer,intent(in)::step

```


* GnuplotPlotContour
```
subroutine GnuplotPlotContour(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep)
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6


```




* GnuplotExportStress
```
subroutine GnuplotExportStress(obj,uvec,sigma,strain_measure,step )
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::uvec(:),sigma(:,:,:),strain_measure(:,:,:)
	integer,intent(in)::step
```

### Requirements
- FEMDomainClass