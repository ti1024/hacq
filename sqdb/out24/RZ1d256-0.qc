# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/256)-i*sin(Pi/256)*Z
# Distance between unitary and approximation:0.00293734
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:54
# T and T^{Dagger} gates:22
# Hadamard gates:22
# Phase and Phase^{Dagger} gates:2
# Pauli gates:8
# Total cost: 22308
# Number of iterations:0
# Approximation time(seconds):0.00154384
# Decomposition time(seconds):0.000245577
# Reduction:0
# sde(abs(z)^2):23
.v 1
.i 1
.o 1
BEGIN RZ1d256(1)
P 1
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
P* 1
H 1
T 1
X 1
END RZ1d256
BEGIN
RZ1d256 1
END
