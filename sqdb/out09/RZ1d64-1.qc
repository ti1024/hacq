# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/64)-i*sin(Pi/64)*Z
# Distance between unitary and approximation:0.0250371
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:82
# T and T^{Dagger} gates:32
# Hadamard gates:31
# Phase and Phase^{Dagger} gates:1
# Pauli gates:18
# Total cost: 32368
# Number of iterations:1
# Approximation time(seconds):0.00140016
# Decomposition time(seconds):0.000303123
# Reduction:8
# sde(abs(z)^2):32
.v 1
.i 1
.o 1
BEGIN RZ1d64(1)
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
T* 1
Z 1
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
X 1
P* 1
END RZ1d64
BEGIN
RZ1d64 1
END
