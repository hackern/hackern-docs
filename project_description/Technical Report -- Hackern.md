# Technical Report -- The Leading Article

> This is the **leading article** of our technical for *Hackern: A Reliable and Lightweight OS For Distributed Computing Based on Functional Language Haskell* project. We also have **sub-articles** relating the more specific details of our work and perspectives.

## Overview

In this report, we will focus on the engineering side of our project, trying to answer the following questions:

* What is the engineering roadmap of the project?
* What are the core engineering challenges of this project?
* How available/usable are the previous projects?
* What are the innovations, from the perspective of engineering?

In practice, what we did in the last week are

* Try to rebuild House OS project (Compilation errors are fixed, but linking is still in stagnation)
* Successfully built the HaLVM (Haskell lightweight Virtual Machine), deployed the Xen Hypervisor, and did some basic testings
* Do some hands-on practice in programming with FFI(Foreign Function Interface) and keep digging into the design of H Monad and H interface (of House)

To avoid the redundancy, we will state the general parts in this article, while writing separate articles on the more specific aspects.

## Engineering Roadmap
### The Core in C/Assembly
* Booting, launch RTS and launch Haskell Kernel
* Interrupt/Exception Interfaces
* Basic memory management
* Low-level I/O port interfacing, memory-mapped I/O, etc.

In this part, the predecessors have some excellent work already, so we will focus on **platform support** and **code verification**.

The platform support is a big problem I think. The House could only run on the bare-metal IA32 architecture, while the Xen-driven HaLVM's author left such a sentence in the github wiki:

> HaLVM works on my machine. And I hope that, it will work on your machine.

So we can try to fix the potential problems in Xen interfaces and automized the whole configuration process.

The verification part is a more advanced plan, which required us to give a **formal description of the underlying hardware**.

### RTS for Haskell in C
* Basic Libraries for Haskell
* Automatic memory management -- GC, no needs to explicitly free allocated memory
* Written in C, adopted from GHC's standard runtime
* Parallel and concurrent supporting, including STM
* Haskell Threads

Also, we don't have to reinvent the RTS. We can improve this part by inventing a **automatic RTS building** tool.

House and HaLVM have all compiled the Haskell RTS into the kernel. But *the major deficiency is the high coupling between other parts with the RTS*.

The GHC is evolving in a fast pace, we want to make a nicer interface between RTS and the hardware-related code, the target is that **we can easily config and compiled the newest RTS of GHC** into the kernel with little or even no modification on the Hackern Code.


### The Kernel in Haskell
* Process Management -- Context as a continuation
* Working with Core and RTS -- Simpler paging handler, easier user-application memory allocation
* Drivers -- Text-mode graphics
* Network Stack -- Uniform Interface
* File System

In this part, we already have the House's code in process management (`ProcessM`), paging system (`PAddr`), the drivers (Intel's network card), the network stack.

But we could do the following original work:

* Port the `Distributed-Haskell` system to Hackern
* Design a NoSQL-DB like network file system which support our distributed computing model and could operate in a **persistent**, **reliable** and **lightweight** way (no needs to support POSIX)

#### More about the FS design
Why it is new? After an in-depth investigation, we found that both House and HaLVM could only operate in the memory pool, with zero capacity in *persistent storage*. To illustrate our design more vividly, let's say an application scenario.

Suppose that you are going to retrieve some data from a remote server, and you have a cluster of *Hackern* machines. The first thing is to *find the most available node* which could fetch the data from remote server and store them persistently.

In this process, the key is to ensure the integrity of task, i.e., you won't mess it up because the network is down. The record of transaction will be kept so you can always restore as long as the network is back.

After that, it will parse the configuration and schedule the work between all currently working nodes. The task might be a "computation kernel", while the data should be parallelized to some extent.

Our idea in this process is that, we can make a shared database-like network file system, which will sync when the network is not busy. When the master nodes is distributing the tasks, it will distributed the "identifier" to the working nodes. The working nodes will use it to poll the data from a universal interface in a "Copy-on-demand" style, like:


	doTask task config identifiers = do
		data <- fsGet identifiers
		let environment = parseConfig config
		execute task environment

We could base our work on the `distributed-process` package, which provides communication, process management and serialization facilities. More importantly, it is asynchronous, which means that it should be more reliable and efficient than a synchronized work model.


### Application Level
* Distributed computing interfaces
    - Map/Reduce Model
    - Serializing function closures for transmission
    - Basid model like Erlang and MPI
    - Fault-tolerant
* A more secure, user-friendly shell
    - POLP: Principle of least privilege
    - Application-centered rather than user-centered: Explicitly apply for authorities


The computing model is the core of this part. We want to mimic the map-reduce computing model to provide a easier-to-program distributed interface, by carefully wrapping the internals of our file system and `distribtuted-haskell` engine, to make the computation more

* Fault-tolerant
* Application centered
* Efficient

## Engineering Challenges
* **The dependency is complex**: RTS(Run-time System) is depending on many C code (`libc` and other non-standard code), which in turn depends on the hardware support, while the OS depends on the RTS as well as the hardware support.
* **The interfaces's design**: Good system API should be both convenient to use, powerful to complete all tasks, while maintaining good insulation, fault handling, and access control. This is a difficult issue for us.
* **Testing and Debugging is subtle**: Compared to application-level code, the system's bugs are harder to trace and fix, and it is more difficult to do unit testing for a system. Luckily we use Haskell as the main language.

## Availability of existing code base

* House is too old to fix, its RTS is very different from the one of current GHC system
* HaLVM lacks any sort of developer document. (But we have contacted its current developer and he is a nice guy.)

## Engineering Innovation
We have discussed the innovation from the perspective of **design and architecture** in the Part II of this series spec, and we will focus on its engineering side in this section. This is a summary of the road map actually, including:

* **Formal verification** of most primitive code
* An **automatic RTS building** tool
* **Port `distributed-Haskell`** platform on a purely Haskell environment
* Add a **novel file system** to the existing OS codebase
* Design a neat **distributed computing interfaces** based on system-level APIs


### Note

> Except for this leading article, we also have written some document on the more specific aspects, please refer to the folders: 

* `Technical Report -- HaLVM.pdf`
* `Technical Report -- House.pdf`

Thanks for reading!
