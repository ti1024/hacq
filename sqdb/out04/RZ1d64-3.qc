# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/64)-i*sin(Pi/64)*Z
# Distance between unitary and approximation:0.0584541
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:109
# T and T^{Dagger} gates:46
# Hadamard gates:46
# Phase and Phase^{Dagger} gates:2
# Pauli gates:15
# Total cost: 46555
# Number of iterations:3
# Approximation time(seconds):0.0192159
# Decomposition time(seconds):0.000391092
# Reduction:256
# sde(abs(z)^2):47
.v 1
.i 1
.o 1
BEGIN RZ1d64(1)
Id 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
P* 1
H 1
T 1
P 1
END RZ1d64
BEGIN
RZ1d64 1
END
