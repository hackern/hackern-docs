Hackern Documentation
---

> Some information for studying

## Background
### Library OS Intro

1. Compile app and kernel to a single standalone image;
2. Built in OCaml, incompatible with POSIX, Apps must be written in OCaml too;
3. Extremely lightweight in code size (DNS server image in 200KB);
4. The performance is comparable to tranditional Linux;
4. Many other advantage during to the type-safe and functional nature.


## Resource

### HaLVM
[Slides](http://www.slideshare.net/xen_com_mgr/the-halvm-a-simple-platform-for-simple-platforms)

[HaLVM github repo](https://github.com/GaloisInc/HaLVM)

[Mail-list](http://community.galois.com/mailman/listinfo/halvm-devel)


### GHC and RTS
[Classical Papers](https://downloads.haskell.org/~ghc/papers/)

### Unikernel
[My First Unikernel](http://roscidus.com/blog/blog/2014/07/28/my-first-unikernel/)

[Unikernels](http://blog.acolyer.org/2015/01/13/unikernels-library-operating-systems-for-the-cloud/)

[Mirage OS](http://openmirage.org) and its [github](https://github.com/mirage/mirage)

### Discussion
[Discussion on SO](http://stackoverflow.com/questions/6638080/is-there-os-written-in-haskell)

[Discussion on Reddit](http://www.reddit.com/r/haskell/comments/29tgjd/ideal_programming_language_for_a_new_modern_os/)

[An immutable operating system](http://augustl.com/blog/2014/an_immutable_operating_system/)

### Language Resource
[ATS language](http://www.ats-lang.org)

[CS240h: Functional Systems in Haskell](http://www.scs.stanford.edu/14sp-cs240h/slides/)

### Related Projects/Works
[The House Project](http://programatica.cs.pdx.edu/House/), [github repo](https://github.com/dls/house), [google site](https://sites.google.com/site/haskell/house-operating-system)

[The Kinetic Project](https://intoverflow.wordpress.com/kinetic/)

[The Verve OS](http://research.microsoft.com/apps/pubs/default.aspx?id=122884)

[Jhc(Ajhc)](http://repetae.net/computer/jhc/)

[L4 micro kernel](http://en.wikipedia.org/wiki/L4_microkernel_family)

[Kernel Modules in Haskell](https://tommd.wordpress.com/2009/09/13/kernel-modules-in-haskell/)


### Distributed
[Clould Haskell](http://haskell-distributed.github.io/documentation.html): A key contribution is a method for serializing function closures for transmission across the network.


### Some toy kernels worth looking at
1. JamesM's UNIX toy kernel [Tutorial](http://www.jamesmolloy.co.uk/tutorial_html/) [Repo](https://github.com/izgzhen/JKernel)
2. MIT's JOS
3. [Hurlex](http://wiki.0xffffff.org)
