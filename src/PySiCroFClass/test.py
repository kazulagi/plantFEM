import SiCroF

world=SiCroF.SiCroF()
world.setDomainList("~/SiCroF/Debug_domainlist.txt")
world.setIfaceList("~/SiCroF/Debug_Ifacelist.txt")
world.exportFortranScript()
