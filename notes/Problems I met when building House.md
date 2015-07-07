# Problems I met when building 

1. autoconf
2. libgmp3-dev
3. /configure --build=i386-unknown-house -> x86_64-unknown-linux
4. FP_CHECK_ALIGNMENT(void *)
5. Local GHC environment

So, possible solutions:

* Compile the GHC seperately
* Read the top-level Makefile
	