# House Project Structure

Top level Makefile does the following things in order:

## Build support

make everything inside the `support` directory, which
includes tiny gcc, math, and gmp libraries. These static libraries
will be used during the building of the kernel.

## Build ghc

This step is quite a mess. But eventually it creates a usable ghc
executable, all the C libraries and includes, and some other
things. This is not very important because we're going to use a newer
version of ghc. However, the C part differs quite a bit from the old
one, which lead to building problems in the next step. We talk about
this later.

## Build kernel

To build the kernel under new ghc, first we need to delete target
`stamp-ghc` from the dependencies of `$(KERNEL)` in the top-level
Makefile. Inside `kernel` directory, we also have to change something
in Makefile. But first, let's see the structure of this Makefile.

The top target is `$(HOUSE)`. It first requires C objects listed in
variable `C_OBJS`. Source code is under `cbits` directory. Compilation
can be finished according to the implicit rules of make. (A *LOT* of
problems occur here, almost all about RTS.)

Then it makes the kernel using `ghc --make`, with self-written linker
`ldhouse`.

NEED MORE DETAILED STUFF HERE.

Some variables to look after here:

* `GHCTOP` and `GHC`: when using our own ghc, these two variables are
  to change. Notice that inside `ldhouse` these variables also exist.

* `CFLAGS` and `ASFLAGS`: for cross compiling, we need to add `-m32`
  to both of these variables.

* `GHCOPTS` and `HOUSE_SRC`: haven't come to this step. To be checked
  later.

Mainline targets are just these above. There are some other targets
that we can build manually, but we don't need to pay too much
attention now.

## (Optional) Build floppy/cdrom

After we get a compiled kernel file, we can use our own tools to
generate ISO file, because house's Makefile almost does the same
thing. This step is simple so we don't have to pay much attention.
