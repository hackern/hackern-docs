# Hackern: A Reliable and Lightweight OS For Distributed Computing Based on Functional Language Haskell


## 1. Background
Nowadays, the information technology is evolving rapidly. In order to build a reliable and lightweight system for some specific task, we have to be innovative, both in bravely choosing the best tool, and in adopting the new ideas and concepts.

First, the tool we choose to build this promising system is different from the classical one. Instead of in traditional languages like C/C++, more and more applications are written in modern languages, such as Java, C#, Python, etc. These languages provide higher-level abstraction for the complicated application logic which is ubiquitous today. Except for the expressiveness and abstraction, safety measures such as strong type and boundary checking help to eliminate potential bugs and protect programs from being exploited by, for example, the notorious buffer overflow attack.

Second, our perspective of the role of OS is different from the old way as well. In today's technology frontier, OS is not the synonym of serving multiple users on the same machine, but includes the concepts of VM, hypervisor and container etc. For example, the hypervisor could provide environment for applications to run separately as cloud service. Expect for virtualization, many other new requirements, such as lightweight in size, easiness in deployment, efficiency in networking, reliability and modularity etc., are pushing us to devise new OS architecture for specific (contrary to "general") but massive usage.

In the last decade, The Programatica Project of Portland State University, The Singularity Project of Microsoft Research, the Xen Project of Cambridge University and so many others, are all making efforts towards such a direction. We want to continue the exploration of a safer, more reliable, and more agile OS development methodology in this semester or beyond.

To conclude, we are trying to combine these new ideas together, to build a lightweight OS kernel for applications written in high level language, esp. Haskell. More importantly, the kernel itself is to be written in Haskell, so as to provide a type-safe environment for the upper-level applications. The final product should be suitable for applications in areas that require high level of both reliability and complex logic, such as finance, airline, and military.

## 2. Motivation
### 2.1 Why an OS, again?
We are not reinventing the wheel, but re-imaging a new kind of vehicle.

As said above, the current environments for cloud services are usually composed of multiple abstract layers, including hypervisors, OS, and language runtime or language VM. In fact, the abstraction and protection are probably overlapping and redundant, which might even harm the system safety on the contrary. By building a kernel specially for a particular kind of applications, such as Haskell programs, we can combine these layers into a slim layer, with high performance and small image size.

By building all necessary supporting functions into the kernel, without redundancy of things such as the POSIX standard, we can redesign the communication protocol and network stack at a lower level, and provide a more effective way of resource management.

Like Unix, which provides a set of easy-to-use and elegant interfaces to program C softwares, we can provide a native functional environment in which running system-level Haskell is easier and more efficient, since the RTS is built directly on hardware.

### 2.2 Why Haskell as a target language?
First, functional langauge is __powerful__, which can be seen from the fact that modern programming languages is borrowing more and more features from functional languages, such as function as first-class citizen, type inference and garbage collection. Many companies, such as Jane Street and Standard Charted, are using functional language to improve productivity and build highly reliable applications.

Second, Haskell is __advanced__. By starting from several simple ideas: purely functional, lazy by default, Hindley–Milner type system with typeclass, it provides high level of abstraction and expressive semantics while maintains the robustness of application by strong type and Monad IO etc. Even from the angle of __efficiency__, Haskell is the one of best in FP, since it has a powerful compiler and RTS tool-set, with many optimization such as TCO (Tail Call Optimization).

Third, functional programming style is __naturally fitting__ into the mindset of parallel computing and distributed computing. It doesn't really matter whether the interface is built in Python or Java, or whether the underlying engine is built in C, JVM. The point is the computing model, in which FP is more natural than imperative language. Here is a code segment from Hadoop, which is "very functional":

	file = spark.textFile("hdfs://...")
	file.flatMap(lambda line: line.split())
    	.map(lambda word: (word, 1))
    	.reduceByKey(lambda a, b: a+b)

If we build the distributed computing in functional language, the computation description could be even more implicit, automatic, and expressive.

Fourth, Haskell is good at being a __DSL__ (Domain-specific language), so you can easily implement the computation model of MapReduce and message model of MPI and Erlang. What's more, Haskell's purity, monad, and closures can greatly expand the potential of distributed computing, which will be discussed at the end of this passage more detailed.

Finally, Haskell has characteristics that are necessary for __serious system level programming__, in particular:

* Strong type
* Static typing
* Compiled efficent code
* FFI (Foreign Function Pointer)
* Monads

And the compiler and runtime (GHC) for Haskell is efficient as well. Even more crucial is that the abstraction it provide helps to engineering the complex logic in the kernel. Finally, by writing the kernel in Haskell, we can provide a set of native Haskell APIs.

### 2.3 Why the cloud/distributed computing domain
Cloud computing and distributed computing are all hot research topic in system domain. By introducing our kernel to the application scenario which requires exactly the robustness and lightweight, we can show the advantages of our system, while avoiding its shortages. What's more, by implementation of the complex computing models on top of our OS, we can effectively demonstrate the power of Haskell as a system-level application language.

Except for the above reasons, Haskell also uniquely introduces some new ideas in distributed computing:

* __Serializing function closures__: Transmitting the function closure as "data", change the way we write distributed computing code .Starting a remote process means sending a representation of a function and its environment across the network.
* __Purity__: As a pure functional language, data is by default immutable, so the lack of shared, mutable data won't be missed. Importantly, immutability allows the implementation to decide whether to share or copy the data. Moreover, pure functions are idempotent.
* __Types and monads__: Types and monads can help to guarantee properties of programs statically. For example, a function that has an externally-visible effect such as sending or receiving a message cannot have the same type as one that is pure. Monadic types make it convenient to program in an effectual style when that is appropriate, while ensuring that the programmer cannot accidentally mix up the pure and effectual code.

## 3. Project Evaluation
> In this section, we will present the evaluation of our project, from the aspects of its vision for future and its important in both academics and industry.

### 3.1 The vision
As mentioned in the background section, our project bears the vision of a new concept of OS. It is reasonable to predict that, this new notion will have big impact on the future of OS.

Due to the decreasing cost of hardware, the overhead caused by additional safety insurance is becoming trivial. Meanwhile, the compilers for modern language constructs are becoming more and more powerful. So, it is reasonable that, most large-scale programs is going to be written in advanced languages such as C#, Java, Scala, Haskell etc. So, a native run-time environment might replace the old general platform in domains like cloud computing.

What's more, computing is coming to everyday life now, including some very sensitive application scenarios, such as military service, trading, banking, medical, manufacturing etc. In these scenarios, data loss, system crush and risk of being attacked are very costly. So the reliability of software will only become more and more vital in the future. The type-safe characteristics of modern language could largely increase the quality of softwares, thus dominating those applications.

### 3.2 Value in academics
From the viewpoint of research value, this project explore the ways of:

* Combining low-level functionality with high-level language constructs
* Combining reliability with lightweight
* Combining the notion of OS with the notion of run-time environment

This exploration can further expand our knowledge of these three aspects, which is highly valuable in research.

### 3.3 Value in industry
Our goal is a OS which provides rapid deployment and efficient execution of Haskell application, esp. in the domain of cloud computing. It could drive the execution of numerous Haskell code.

A brief list of possibly hosted applications.

* Trading, such as
    - Deutsche Bank Equity Proprietary Trading
    - Starling Software Tokyo, Japan
* Banking, such as
    - ABN AMRO Amsterdam, The Netherlands
    - Standard Chartered
* IT
    - Hoogle: A Haskell API search engine
    - Haxl: Facebook's project of data processing
    - Ganeti: Google's project of managing clusters of virtual servers built on top of Xen and KVM.
    - Intel's research on multicore parallelism at scale.
    - Bond: Microsoft's production serialization system, broadly used at Microsoft in high scale services. 
* Space
    -  Misson Control
    -  Satellite operations
    -  Linspire/Freespire - Haskell for systems programming

Even more can be found at [CUFP (Commercial-Users of Functional Programming)](http://cufp.org/2015/).


## 4. Related Work
### 4.1 System-related Projects
#### House
House is a monolithic Haskell OS, introducing the "H interface" between hardware and application, with primitive GUI system.

It is a research project belonging to The Programatica Project, which aims at __high assurance software__. The original paper is published ten years ago, while the last release is 5 years ago.

The highlights of this OS are two points:

1. Using P-Logic to verify the system code in Haskell
2. Develop a neat interface between application and hardware called H Interface, which maintains the separation of side-effect (DMA driver is the only potential hole in H's memory safety.)

#### Osker
Osker is actually the sibling project of House. It is a L4 kernel, and different from House is the following aspects:

* Explicit approach to concurrency
* Supports IPC (interprocess communication)

#### Verve:
Verve is rather new (2011). It is part of the Singularity Project, which is lead by Microsoft Research, aiming at _construction of dependable systems through innovation in the areas of systems, languages, and tools_.

Verve has a Boogie verified "Nuclesus" in assembly language and a TAL verified Kernel in C#. They have developed techniques and tools to mechanically verify the safety of every assembly-language instruction in the operating system, runtime system, drivers, and applications (in fact, every part of the system software except the boot loader).

#### Mirage OS
This is an OCaml OS, emphasizing the __efficiency of execution__, adopting the idea of "Library OS", or "unikernel".

The libOS architecture has several advantages over more conventional designs. For applications where performance and especially predictable performance is required, a libOS wins by allowing applications to access hardware resources directly without having to make repeated privilege transitions to move data between user space and kernel space.

The libOS does not have a central networking service into which both high-priority network packets (such as those from a video conference call) and low-priority packets (such as those from a background file download) are forced to mix and interfere. Instead, libOS applications have entirely separate queues, and packets mix together only when they arrive at the network device.

#### More projects
* Kinetic: Expressiveness guided, use Monads excessively
* Halfs: the Haskell Filesystem
* HaLVM: An example of Haskell running natively without an operating system underneath

### 4.2 Distributed/cloud-related Projects
* Glasgow Distributed Haskell (GdH), a specialize __compiler__
* __Distributed__ Library and __Concurrent__ Library of Haskell
* __Scala__, which is a combination of OOP and FP, is heavily used in __Spark__
* __MBrace__: An open source framework for large-scale distributed computation and data processing written in F#

Here is a more detailed demonstration of the "Cloud Haskell" DSL system.

First, The basic unit of concurrency in Cloud Haskell is the __process__: a concurrent activity that has been "blessed" with the ability to send and receive messages. As in Erlang, processes are lightweight, with low creation and scheduling overhead.

Second, Most of the data structure is __serializable__ in Cloud Haskell, including functions closure.

Third, Cloud Haskell is highly __fault-tolerant__, one process can monitor another process; if the monitored process terminates, the monitoring process will be notified. Ascertaining the origin of the failure and recovering from it are left to the application or a higher-level framework.

What's more, processes can accept messages of multiple specified types through __"message matching"__. And to take advantage of Haskell's strong type, Cloud Haskell provides distributed __typed channels__ as an alternative to sending messages directly to a process. Each channel consists of two ends, which we call the send port and the receive port. And a channel is related to a particular type.

From the perspective of performance, results show that Cloud Haskell has performance roughly __comparable with that of the Hadoop framework__, and in some cases superior. Although the Hadoop implementation performs better with fewer nodes, the Cloud Haskell version overtakes it as more nodes are added, and retains this lead. The greatest bottleneck in Cloud Haskell's performance is acquiring and loading the data; we hope to address this issue by improving file handling.

### 4.3 FP in Industry
* __Jane Street__, a quantitative trading firm, contributed `Core`, an alternative to the OCaml standard library, and `Async`, a sophisticated library for __concurrent programming__.
* __Standard Charted__, the famous bank, owns the largest codebase in Haskell, roughly 1.3 million lines of Haskell for trading, including a compiler for their home-grown Haskell dialect Mu.
* __Facebook__ has developed `Haxl`, a Haskell library that simplifies access to remote data, such as __databases or web-based services__

To quote Jane Street in why it uses OCaml intensively, it is also our intention of using Haskell:

> We've taken a different view. We believe that functional programming boosts our productivity, and we don't mind going our own way. When you're solving hard problems, there's no substitute for using the best tools available.

> OCaml in particular provides a great combination of qualities. It's streamlined and efficient, making it possible to achieve performance near that of C. At the same time, its powerful type system acts as a rich and well-integrated set of static analysis tools that help you improve the quality of your code, catching bugs at the earliest possible stage. Billions of dollars of transactions flow through our systems every day, so that means a lot to us.

> OCaml is also highly expressive, letting you convey your ideas clearly, precisely and readably. We believe that if you want to build high quality, robust software, readability is essential.

## FAQ
> This section presents some common questions of this project as well as its answer.

### Will langauge effect the robustness of OS?
Yes.

Although the traditional kernels like Unix/Linux in a unsafe language C is robust and stable today, but we can't neglect the decades of updates, patches and maintenance. In the very first years of Unix's popularity, it also had memory management problem and always crushed, only dumping a lot of meaningless debugging information.

Actually, in the last century, the choices of programming languages are rather limited, with only C/C++ the best choice at that time, although this might introduce numerous potential bugs. Even today, CERT's database is still full of security holes in Unix/Linux platform.

But today, many new languages are invented to build more robust system. For example, Go and Rust are all very young, system-level programming languages. In comparison, C is no longer a good choice for building reliable systems. C is notorious for its lack of strong-type, boundary-checking, as well as easy memory leaking, low expressiveness etc. All trade-offs for efficiency. An average programmer is easy to write some buggy code even for a simple linked list. But making mistake would be much harder, if using a more safe, high-level, and expressive language.

In conclusion, language is the way we express our ideas in building a system. If we choose C, we must be very attentive to the possible bugs, being limited in a small syntax, and forced to do many things manually. But if in a high-level language, we can focus more on the algorithms, the architecture, the ideas, while producing high quality, robust and maintainable code with the help of machine.


### Why Haskell (or ML, OCaml etc) is not popular?
There are primary two reasons, but they are not necessarily the deficiencies of the language itself.

* FP is difficult to learn for a programmer who used C or OOP for a long time. So the training cost is too high for the common companies. (Although there are more and more Haskell programmer being hired)
* FP community is somewhat isolated in academics, and those PhDs are always too lazy to market FP.

However, the truth is that, Haskell is productive, powerful, safe, and compiled. As the emergence of F#, Scala etc., Haskell is being more and more popular these times.

### What are the potential challenges/problems of using Haskell in system?
Nothing is perfect, so is Haskell. Here we demonstrate the biggest hurdles as well as possible solutions in using Haskell to develop such a system.

1. Laziness might incur high space overhead in some scenarios, and make it hard to analyze the space/time complexity. The solution could be first to investigate the "bad code", and then using its strict version instead.
2. Kernel-level garbage collection might introduce complexity in interrupt handling. The solution could be borrowed from House system, i.e., to poll the interrupt actively after GC.


## The future work
In the Part II of the project specification, we will present in more details of implementation, to answer questions like:

1. What is the theory basis of our design?
2. What is the technology basis of our design?
3. What are the innovative sides of our design?

And we will begin to write more about the engineering, expand the fellowing draft into a more detailed and plausible one:

### Engineering [Draft]
#### The Core in C/Assembly
* Booting, launch RTS and launch Haskell Kernel
* Interrupt/Exception Interfaces
* Basic memory management
* Low-level I/O port interfacing, memory-mapped I/O etc.
* User/Kernel Mode

#### RTS for Haskell in C
* Basic Libraries for Haskell
* Automatic memory management -- GC, no needs to explicitly free allocated memory
* Written in C, adopted from GHC's standard runtime
* Parallel and concurrent supporting, including STM
* Haskell Threads

#### The Kernel in Haskell
* Process Management -- Context as a continuation
* Working with Core and RTS -- Simpler paging handler, easier user-application memory allocation
* Drivers -- Text-mode graphics
* Network Stack -- Uniform Interface
* File System

#### Application Level
* A more secure, user-friendly shell
    - POLP: Principle of least privilege
    - Application-centered rather than user-centered: Explicitly apply for authorities
* Distributed computing interfaces
    - Map/Reduce Model
    - Serializing function closures for transmission
    - MP model of Erlang and MPI for HPC
    - Fault-tolerant

Thank you!
