# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/16)-i*sin(Pi/16)*Z
# Distance between unitary and approximation:0.00134296
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:74
# T and T^{Dagger} gates:28
# Hadamard gates:27
# Phase and Phase^{Dagger} gates:2
# Pauli gates:17
# Total cost: 28367
# Number of iterations:0
# Approximation time(seconds):0.131065
# Decomposition time(seconds):0.000285163
# Reduction:0
# sde(abs(z)^2):28
.v 1
.i 1
.o 1
BEGIN RZ1d16(1)
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
P 1
H 1
T 1
P 1
H 1
T 1
X 1
END RZ1d16
BEGIN
RZ1d16 1
END
