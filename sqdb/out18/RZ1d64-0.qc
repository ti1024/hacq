# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/64)-i*sin(Pi/64)*Z
# Distance between unitary and approximation:0.00798858
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:40
# T and T^{Dagger} gates:16
# Hadamard gates:17
# Phase and Phase^{Dagger} gates:1
# Pauli gates:6
# Total cost: 16216
# Number of iterations:0
# Approximation time(seconds):9.6399e-005
# Decomposition time(seconds):0.000214789
# Reduction:0
# sde(abs(z)^2):18
.v 1
.i 1
.o 1
BEGIN RZ1d64(1)
P 1
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
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
END RZ1d64
BEGIN
RZ1d64 1
END
