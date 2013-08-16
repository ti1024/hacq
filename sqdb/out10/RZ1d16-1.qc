# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/16)-i*sin(Pi/16)*Z
# Distance between unitary and approximation:0.0247967
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:102
# T and T^{Dagger} gates:38
# Hadamard gates:38
# Phase and Phase^{Dagger} gates:3
# Pauli gates:23
# Total cost: 38523
# Number of iterations:1
# Approximation time(seconds):0.00207788
# Decomposition time(seconds):0.000346741
# Reduction:10
# sde(abs(z)^2):39
.v 1
.i 1
.o 1
BEGIN RZ1d16(1)
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
P* 1
H 1
T 1
X 1
P* 1
END RZ1d16
BEGIN
RZ1d16 1
END
