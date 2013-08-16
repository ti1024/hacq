# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/32)-i*sin(Pi/32)*Z
# Distance between unitary and approximation:0.0147913
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:41
# T and T^{Dagger} gates:16
# Hadamard gates:16
# Phase and Phase^{Dagger} gates:1
# Pauli gates:8
# Total cost: 16208
# Number of iterations:0
# Approximation time(seconds):7.294e-005
# Decomposition time(seconds):0.00020196
# Reduction:0
# sde(abs(z)^2):17
.v 1
.i 1
.o 1
BEGIN RZ1d32(1)
Id 1
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
P 1
END RZ1d32
BEGIN
RZ1d32 1
END
