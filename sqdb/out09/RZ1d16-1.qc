# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/16)-i*sin(Pi/16)*Z
# Distance between unitary and approximation:0.0240005
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:92
# T and T^{Dagger} gates:34
# Hadamard gates:34
# Phase and Phase^{Dagger} gates:3
# Pauli gates:21
# Total cost: 34482
# Number of iterations:1
# Approximation time(seconds):0.00237624
# Decomposition time(seconds):0.000316318
# Reduction:14
# sde(abs(z)^2):35
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
P 1
H 1
T 1
P 1
H 1
T 1
Y 1
END RZ1d16
BEGIN
RZ1d16 1
END
