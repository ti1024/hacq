# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/128)-i*sin(Pi/128)*Z
# Distance between unitary and approximation:0.0163652
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:43
# T and T^{Dagger} gates:16
# Hadamard gates:16
# Phase and Phase^{Dagger} gates:1
# Pauli gates:10
# Total cost: 16210
# Number of iterations:0
# Approximation time(seconds):0.000101896
# Decomposition time(seconds):0.000207458
# Reduction:0
# sde(abs(z)^2):17
.v 1
.i 1
.o 1
BEGIN RZ1d128(1)
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
P 1
END RZ1d128
BEGIN
RZ1d128 1
END
