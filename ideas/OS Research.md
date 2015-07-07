OS Research
===

* SHILL: A Secure Shell Scripting Language
* VirtuOS: an operating system with kernel virtualization
* Safe to the last instruction: automated verification of a type-safe operating system


## SHILL
### Intro
* SHILL is a secure shell scripting language.
* Security is accomplished by "sandboxing", "language design" and "contracts with consumers"
* Implementation: Developed with Racket, run on FreeBSD prototype
* Paper from: USENIX 14' [PDF](https://www.usenix.org/system/files/conference/osdi14/osdi14-paper-moore.pdf), [lab](http://shill.seas.harvard.edu)

![](SHILL.png)

### Evaluation
* The code is open-sourced on GitHub, and the development is in progress
* Related to several aspects, including OS, Sandboxing, Language Design etc.

### What could be done with it?
* We could include more features, such as resource accouting etc.
* Since SHILL is based on MAC(mandantory access control), we can migrate it to DAC(Discrete Access Control) mechainism
* It is only running on BSD currently, we could port it to the Linux environment
* We could close these [issues](https://github.com/HarvardPL/shill/issues)

## VirtuOS
### Intros
* Kernel-level decomposition to prevent disasters
* Use virtualization techniques to provide isolation between components
* Implemented Protypes: Linux kernel and Xen hypervisor
* Exceptionless, fault-resilient

![](VirtuOS.png)

### Evaluation
* Open source
* [Homepage](http://people.cs.vt.edu/~rnikola/?page_id=246)
* The development seems stalled (R1.01 in August 2013)

### What we could do with it?
* Solving the problems addressed in the paper
	* The vertical decomposition of a monolithic kernel makes the implementation of calls that intertwine multiple sub- systems difficult.
	* Our prototype also does not support mmap for file descriptors serviced by the storage domain.
	* Our current prototype also does not provide trans- parency for file path resolution when a storage domain is involved.
	* supporting system-wide policies or resource sharing
* Make it more lightweighted, increase its I/O performance.

## Type-safe operating system
### Intros
* We use TAL and Hoare logic to achieve highly auto-mated, static verification of the safety of a new operating system called Verve.
* The Nucleus, written in verified assembly language, implements allocation, garbage collection, multiple stacks, interrupt handling, and device access. The kernel, written in C# and compiled to TAL, builds higher-level services, such as preemptive threads, on top of the Nucleus. A TAL checker verifies the safety of the kernel and applications. 
* [wikipedia](http://en.wikipedia.org/wiki/Verve_(operating_system))

![](Verve.png)

### Evaluation
* Very intersting!!!

### What we could do with it?
* Rewrite the kernel using other language, such as, Haskell :)
* Add some Real-time features to it, make it work on something like a UAV or robot.
* Add some networking features to it, as a "robust & native" support for server software