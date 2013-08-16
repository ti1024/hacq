# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/256)-i*sin(Pi/256)*Z
# Distance between unitary and approximation:0.00850106
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:86
# T and T^{Dagger} gates:34
# Hadamard gates:35
# Phase and Phase^{Dagger} gates:2
# Pauli gates:15
# Total cost: 34445
# Number of iterations:1
# Approximation time(seconds):0.00144744
# Decomposition time(seconds):0.000327681
# Reduction:12
# sde(abs(z)^2):36
.v 1
.i 1
.o 1
BEGIN RZ1d256(1)
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
T 1
P 1
H 1
P* 1
END RZ1d256
BEGIN
RZ1d256 1
END
