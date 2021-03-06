This directory contains:
* Hacq, a library to describe quantum circuits in Haskell, and
* several sample programs for Hacq
* Circuit database for certain single-qubit phase shifts (in directory "sqdb").
  These circuits were generated by using Single Qubit Circuit Toolkit (SQCT),
  which is written by Vadym Kliuchnikov, Dmitri Maslov, and Michele Mosca.
  For the instructions about how to regenerate the circuit database,
  read below under the heading "Preparation of circuit database by SQCT".

# How to run

1. Install Haskell Platform: <http://hackage.haskell.org/platform/>.
   Generally it is better to use the newest release of Haskell Platform.
   Hacq has been tested with Haskell Platform 2013.2.0.0.

2. Install cabal-dev, by executing the following from the shell:

    cabal update
    cabal install cabal-dev

3. After setting this directory (where hacq.cabal is located) as the current directory, install the Haskell libraries used by Hacq, by executing the following from the shell:

    cabal-dev install unordered-containers fingertree blaze-builder hashtables

   This creates a directory named "cabal-dev" and installs dependencies inside it.

4. With the same current directory, run

    cabal-dev configure
    cabal-dev build
    cabal-dev install

   This builds and installs Hacq in directory "cabal-dev".

5. To compile sample executables, go to directory examples and run

    cabal-dev configure --sandbox=../cabal-dev
    cabal-dev build --sandbox=../cabal-dev

   This creates executables in the directory dist/build/<sample-name>.

   Among the executables created in step 5, "fourier" requires the
   path to the directory of the circuit database.
   The "sqdb" directory of Hacq contains this database.
   To run fourier, use the following syntax:
   $ fourier -d sqdb
   where sqdb is the path to the circuit database directory.

6. To produce the HTML documentation of the exported functions in each module, run

    cabal haddock --executables

   This creates dist/doc/html/hacq/<executable-name>/index.html.

# Generation of circuit database by SQCT

The circuit database in directory sqdb is generated by using
Single Qubit Circuit Toolkit (SQCT), which is written by Vadym Kliuchnikov,
Dmitri Maslov, and Michele Mosca and available at http://code.google.com/p/sqct/.
Here is how to generate the circuit database using SQCT.

After compiling SQCT, first run,
$ sqct --epsilon-net nnn
where nnn is an integer at most 30.
Specifying --epsilon-net 30 requires 64-bit version of SQCT and a lot of memory.
(It took 4 minutes to complete on a Windows PC with 8 GB RAM and Core i7 2.8 GHz.)
This creates the epsilon-net files for SQCT up to epsilon-net.nnn.bin.

Then run
> mkdir out03
> sqct --dotqc --out-dir out03 --max-sde 3 --in sqct.in
> mkdir out04
> sqct --dotqc --out-dir out04 --max-sde 4 --in sqct.in
...
up to nnn, where sqct.in is the file in this directory.
This produces .qc files inside directories out03, out04, ....
