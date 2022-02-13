import numpy as np
import scipy.sparse
import scipy.linalg
import scipy.sparse.linalg
import pandas as pd


K = np.loadtxt("K_dense.csv",dtype="float32",delimiter=",")
M = np.loadtxt("M_dense.csv",dtype="float32",delimiter=",")

K_crs = scipy.sparse.csr_matrix(K)
M_crs = scipy.sparse.csr_matrix(M)

print(np.array_equal(K, K.T))
print(np.array_equal(M, M.T))

eig_val,eig_vec  = scipy.sparse.linalg.lobpcg(K_crs,M_crs,tol=1.0e-20,
maxiter=100000,largest=False)

sorted_val = np.sort(np.sqrt((eig_val))/3.14159/2.0 )

for mode_id in range(len(eig_vec[:,1])):
    f = open("U_"+str(mode_id)+".txt","w")
    
    mode_vec = eig_vec [:, mode_id]
    #mode_vec = eig_vec [mode_id, :]
    
    for i in range(len(mode_vec) ):
        f.write(str(mode_vec[i])+"\n")
    f.close()

f = open("eigen_val.txt","w")
for i in range(len(eig_val) ):
    f.write(str(eig_val[i])+"\n")
f.close()
