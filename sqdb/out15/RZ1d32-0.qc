# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/32)-i*sin(Pi/32)*Z
# Distance between unitary and approximation:0.017346
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:30
# T and T^{Dagger} gates:12
# Hadamard gates:11
# Phase and Phase^{Dagger} gates:1
# Pauli gates:6
# Total cost: 12156
# Number of iterations:0
# Approximation time(seconds):2.969e-005
# Decomposition time(seconds):0.000170072
# Reduction:0
# sde(abs(z)^2):12
.v 1
.i 1
.o 1
BEGIN RZ1d32(1)
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
T 1
H 1
T 1
Z 1
END RZ1d32
BEGIN
RZ1d32 1
END
