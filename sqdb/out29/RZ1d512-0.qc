# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/512)-i*sin(Pi/512)*Z
# Distance between unitary and approximation:0.00106268
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:69
# T and T^{Dagger} gates:28
# Hadamard gates:28
# Phase and Phase^{Dagger} gates:0
# Pauli gates:13
# Total cost: 28294
# Number of iterations:0
# Approximation time(seconds):0.0519008
# Decomposition time(seconds):0.000274167
# Reduction:0
# sde(abs(z)^2):29
.v 1
.i 1
.o 1
BEGIN RZ1d512(1)
Id 1
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
T 1
H 1
T 1
H 1
T 1
Y 1
END RZ1d512
BEGIN
RZ1d512 1
END
