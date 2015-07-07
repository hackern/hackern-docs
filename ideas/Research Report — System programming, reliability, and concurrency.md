# Research Report — System programming, reliability, and concurrency

## FAQs
### 操作系统的脆弱健壮与否与编程语言没有直接关系?
Yes, they are directly relevant.

First, Unix are not born robust. Unix/Linux platform is very robust and stable now, but this is due to decades of updates, patches and maintainance. In the very first years of Unix's popularity, it also has memory management problem and always crushed, just dumping a lot of meaningless debugging information. And in the last century, the choice of programming languages are rather limited. So C/C++ is already best choice at that time, although might introduce numerous bugs. Even today, the CERT's database is still full of security holes in Unix/Linux platform.

Second, many languages are invented to build more robust system. For example, Go and Rust are all very young, system-level programming languages.

Third, C is really a bad choice for building a reliable system. C is notorious for its lack of strong-type, boundary-checking, as well as easy memory leaking, low expressiveness etc. All trade-offs for efficiency. An average programmer is easy to write some buggy code even for a simple linked list. But making mistake would be much harded, if using a more safe, high-level, and expressive language.

In conlucsion, Language is the way we express our ideas in building a system. If we choose C, we must be very attentive to the possible bugs, being limited in a small syntax, and forced to do many things manually. But if in a high-level language, we can focus more on the algorithms, the architecture, the ideas, while producing high quality, robust and maintainable code with the help of machine.

So, we believe that the language will directly effect the system you are trying to build.

### 并行/分布式在目前的平台上也得到了很好的实现，为什么一定要用 Haskell

First, functional programming style is naturally fitting into parallel computing and distributed computing.

It doesn't really matter whether the interface is built in Python or Java, whether the underlying computing is built in C, JVM, or something else. But from the perspective of computing model, FP is more natural than imperative language. Here is a code segment from Hadoop, which is "very functional":

    file = spark.textFile("hdfs://...")
     
    file.flatMap(lambda line: line.split())
        .map(lambda word: (word, 1))
        .reduceByKey(lambda a, b: a+b)

So, If we build the distributed computing in functional language, the computation description could be even more implicit, more automatic, and more abstract.

Second, Haskell is powerful as a DSL (Domain-specific language), so you can easily implement the computation model of MapReduce and message model of MPI and Erlang. What's more, Haskell's purity, monad, and closures can greatly expand the potential of distributed computing, which will be discussed at the end of this passage more detailed.


### Haskell 为什么没有流传开来?
Haskell does has some drawbacks, while __these drawbacks might not be the major reason why it is not widely used__.

#### Haskell's drawbacks
* __Laziness__ might incur high space overhead in some scenarios, and make it hard to analyze the space/time complexity.
* C's low-level operation, bit-twiddling, and direct memory (state) manipulating is difficult to express in Haskell, or any purely functional languages

But we can explicitly use strict evaluation in Haskell to fix the first problem, and use C interfacing to fix the second one, while preserving the seperation between pure and impure code.

#### Why Haskell (or ML, OCaml etc) is not popular?
* FP is difficult to learn for a programmer who used C or OOP for a long time. So the training cost is too high for the common companies. (Although there are more and more Haskell programmer being hired)
* FP community is somewhat isolated in academics, and those PhDs are always too lazy to market FP.

However, the truth is that, Haskell is productive, powerful, safe, and compiled. As the emergence of F#, Scala etc., Haskell is being more and more popular these times.

#### 给出的更多的是编程语言的优点，但和操作系统的关系有多大？所提的很多优点其实在应用层已经有很好的实现，那么为什么要做到os层面，其意义何在？

First, **we can promise the safety and quality of the whole system**. This important if you are writing a software for financial, military and other scenarios which demands high reliability.

Second, system calling is more natural to use. Sometimes, if you want to do system programming in Haskell, you are forced to write many quirky Haskell code to do interfacing with underlying OS.

Like Unix, providing a set of easy-to-use and elegant interfaces to program C softwares, we can provide a native functional environment in which writing system-level Haskell is more easier, and more efficient (Since the RTS is built directly on hardware)

Third, as a distributed computing system, we can design from the communication protocol and network stack, building a more effective way of resource management underlying model.

Finally, __OS is changing fast__, we can't limit our imagination only on the conventional Unix/Linux architecture. There are many unconventional OS, such as the Hypervisor Xen, JVM on bare metal, Library OS Unikernel etc.

For example, unikernel, in which OS, application and configuration can be compiled and linked statically as a whole, which optimized the VM's efficiency greatly and can be spawned and deployed instantly. What's more, it build the OCaml type-safe protocol I/O in OS, which could integrete seamlessly with high-level application, leading to a more reliable and safe product.

### Why use Haskell in system programming rather than other high-level language?

To achieve the goal, the language has to be:

* Static typing (efficient)
* Strong typing (safe)
* Compiled (efficient and native)

So, we can only choose from Haskell, OCaml, Golang, Rust, C#, F#, ML.

In these options, Haskell is the most familiar one to our team, and only Haskell and OCaml have some classical cases in OS research.


## Engineering
### The Core in C/Assembly
* Booting, launch RTS and launch Haskell Kernel
* Interrupt/Exception Interfaces
* Basic memory management
* Low-level I/O port interfacing, memory-mapped I/O etc.
* User/Kernel Mode

### RTS for Haskell in C
* Basic Libraries for Haskell
* Automatic memory management -- GC, no needs to explicitly free allocated memory
* Written in C, adopted from GHC's standard runtime
* Parallel and concurrent supporting, including STM
* Haskell Threads

### The Kernel in Haskell
* Process Management -- Context as a continuation
* Working with Core and RTS -- Simpler paging handler, easier user-application memory allocation
* Drivers -- Text-mode graphics
* Network Stack -- Uniform Interface
* File System

### Application Level
* A more secure, user-friendly shell
    - POLP: Principle of least privilege
    - Application-centered rather than user-centered: Explicitly apply for authorities
* Distributed computing interfaces
    - Map/Reduce Model
    - Serializing function closures for transmission
    - MP model of Erlang and MPI for HPC
    - Fault-tolerant
