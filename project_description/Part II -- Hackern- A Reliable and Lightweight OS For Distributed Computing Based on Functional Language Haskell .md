# Hackern: A Reliable and Lightweight OS For Distributed Computing Based on Functional Language Haskell 
> Part II of Project Specification: The Feasibility Analysis


## 1. Theory Basis
> All projects have its theory basis, here we will draw the most important ones of them.

### 1.1 State Model in Haskell
Haskell is famous for being a _purely functional language_, which aims at separation of __pure code__ and __impure code__.

In the __pure__ world, functions are stateless, i.e., as long as inputs are same in __value__, the output will always be same. So, we won’t have any surprise, since the behavior of pure code are strictly defined.

But impure code, are not so easy to determine. However, without the stateful code, we change effect the real world, since the __world is itself stateful__. For example, I/O will cause the change I/P appliances’ state, preventing the race condition will need a “lock” state. And in the famous language C, state are the core programming paradigm, like:

	int a = 10;
	int b = a;
	printf("%d", a);

Every line of this little segment of code will explicitly cause change of state!

But the Haskell code still needs to communicate with the outer world, in the way of __IO Monad__ and __main__ function.

#### 1.1.1 IO Monad and main function
The monad is actually a powerful way of _transforming the non-stateful code into a quasi-stateful code_, by introducing special construct, notations and syntax sugar.

So, we can write code in Haskell like this:

	outputNum = do
		let a = 1
		let b = 2
		putStrLn $ show (a + b)

which is just like C code!

However, this segment of code is not causing any real effect as well, just a special construct of usual Haskell function. But if we put it into the magical __main__ function, the IO Monad will cause the real effect!

	main = do

Except for IO Monad, which is heavily used in IO operations such as printing a line and generating a random number, there are also things like

* State Monad
* Writer Monad
* Reader Monad

which could help us write __monadic__ code.

### 1.2 OS and Software Stack
What is OS?

> According to wikipedia, an operating system (OS) is software that manages computer hardware and software resources and provides common services for computer programs. The operating system is an essential component of the system software in a computer system. Application programs usually require an operating system to function.

So, actually, OS is no more than a "resource manager + scheduler + service provider", which abstract the underlying hardware layer.

But, a bare kernel can't really be put into production today!!! To run a application, for example, in Java, we needs to:

* Download the distribution of Linux (Not the kernel! A lot of stuff have already been built-in)
* Install necessary softwares, such as `build-essentials`, `sshd` etc.
* Download and Install the JRE, JVM etc.

Finally, we can run the program!!!

So, to run a user application, which is usually written in high-level language, we generally need a stack of applications:

* Container or Hypervisor etc.
* Kernel
* User space OS facilities
* Library / Runtime / VM
* User Interface, such as shell or GUI

So, OS can be seen as part of software stack for an application, while some more softwares has to be installed further as another part of the stack.

It is worth nothing that, although this architecture seems redundant and inefficient, it is highly layered and with good interfaces between each other, so it is highly __generic__ for various requirement of the user.

### 1.3 Haskell Runtime Environment
Haskell, as a language, also needs a runtime environment. Or, your compiled code can use functions in `Prelude`, `Concurrency` or more.

However, different from the environment for a C program (which is essentially a big library), GHC's runtime is very "special", since there are so many __special__ features in Haskell which the runtime needs to support. Some of these features, such as __lazy evaluation__ and __ubiquitous concurrency__, an emphasis on __pure__ computation without mutation.

In general, GHC RTS has components like:

* Garbage Collector (Abstract of storage)
* Thread Scheduler
* Profiler
* Error handlers
* Byte-code interpreter
* Software Transactional Memory (STM) support

They are all very important from the perspective of operating system. Since these components are actually also like OS's

* Virtual Memory and Paging
* Process Scheduler
* Interrupt and Exception Handler

Expect for these, things like standard libraries are also part of the Haskell environment, although not mandatory.


### 1.4 Distributed Computing Model in Haskell
The classical computing model in Haskell in built on top of Erlang's message passing model, combining the semantics of both Concurrency and parallelism.

In this model, scalability and fault-tolerance are important factors.

To make on-demand computation efficient, it made sense for processes to be started very quickly, to be destroyed very quickly and to be able to switch them really fast. Having them lightweight was mandatory to achieve this. It was also mandatory because you didn't want to have things like process pools. so, it would be much easier to design programs that could use as many processes as they need. Another important aspect of scalability is to be able to bypass your hardware's limitations, so you can always make it sufficient for the demands.

Reliability is also crucial. Being reliable is not to make no mistakes (although this is the holy grail), but to recover and get the mission done even there are mistakes and errors. In current world of computation, failure is common. You can try to prevent bugs all you want, but most of the time some of them will still happen. In the eventuality bugs don't happen, nothing can stop hardware failures all the time. The idea is thus to find good ways to handle errors and problems rather than trying to prevent them all.

So, with these two requirements, we will have some basic distributed computing models:

In Erlang, the communication model (among processes) is __message passing__. No much need to be said about this. Erlang processes __share no memory__. The way that processes communicate is via (asynchronous) message passing. Every process (even the shell) holds a mailbox queue where incoming messages are placed until received by the process. Message passing is __asynchronous__ because the sending process does not block on send. On the other hand, receiving a message in Erlang is a blocking operation.

Based on this, the computing model of Haskell adds something like:

* Functions can be sent
* Programmable serialization

## 2. Engineering Basis
> In this section, we will introduce to your the engineering considerations we have.

### 2.1 FFI and H Monad
#### 2.1.1 FFI
FFI (Foreign Function Interface) is the way Haskell _calls functions in code written in other language_. This is also _the way to combine the advantages of Haskell and C_.

Compared to many other languages, Haskell FFI is very easy to use: in the most common case, you only have to translate the prototype of the foreign function into the equivalent Haskell prototype and you're done. For instance, to call the exponential function ("exp") of the libc, you only have to translate its prototype:

    double exp(double);

into the following Haskell code

    foreign import ccall "exp" c_exp :: Double -> Double

However, binding one language to another is a nontrivial task. The binding language needs to understand the calling conventions, type system, data structures, memory allocation mechanisms, and linking strategy of the target language, just to get things working. The task is to carefully align the semantics of both languages so that both can understand the data that passes between them.

The interesting thing is, for pure C code, we bind it to pure Haskell code, while for C code with side effect, we always wrap it in Monad to separate the problem.

A example segment of code:

	foreign import ccall "pcre.h pcre_fullinfo"
	    c_pcre_fullinfo :: Ptr PCRE
	                    -> Ptr PCREExtra
	                    -> PCREInfo
	                    -> Ptr a
	                    -> IO CInt

#### 2.1.2 H Monad
The H Monad is the way we wrap the hardware into the way easier to program with Haskell. As we've said before, Monad can represent the stateful operations, which is exactly what is happening inside the hardware, there is an example segment of code of House, about task scheduling.

	tScheduler :: (HMonad m, StateMonad System) => Domain -> tScheduler
	dom = case runnable dom of       []     -> return ()       (t:ts) -> do dom’ <-                     execThread (uproc dom) t                      ‘runStateTs‘ dom{runnable=ts}                    update (insertDomain dom’)

### 2.2 RTS implementation
This is always a engineering challenge, although people has succeeded on it. There are some considerations:

1. __Use what RTS?__ The GHC RTS (which is the de facto standard), some special but more portable RTS, or even write a RTS from scratch?
2. __How to transplant?__, if we use the standard RTS of GHC, we must port it from the standard production environment to the "bare-metal" environment, in which it can:

* Directly manipulate data in memory
* Direct access to the I/O ports and memory-mapped regions
* Direct access to paging hardware and protection mode switching


So, if we want to move the RTS, the code needs to be extended to initialize the bare machine. The remainder of the RTS is also needed to be scrubbed to remove any dependencies on an underlying operating system, in which an important part is `libc` support.


### 2.3 Interfaces for applications
#### 2.3.1 The language API
Briefly speaking, the interface we provides should contain at least what a normal Haskell could provides _in the form of standard library_ according to the newest Haskell spec _haskell2010_, which might include things like:

* Standard Data Types
* Strict Evaluation
* Standard Haskell Classes
* Basic Input/Output
* Exception Handling Mechanism

They might be appearing in form of importable modules, like

* `Data.Char`
* `Control.Monad`
* `System.IO`

#### 2.3.2 The distributed system API
Except for the standard library, we will implement a special system-level API for the underlying distributed computing models. They will also be imported as common modules, like

	import Distributed.Hackern

But due to the slim software stack and system-level support, it could be faster and more natural.

As a vivid illustration, the code could be written like this:

	ping :: ProcessM ()
	ping = do Pong partner <- expect
		self <- getSelfPid
		send partner (Ping self)
		ping

Or, some map-reduce style:

	master :: [Input] -> [NodeId] -> Process ()
	master inputs workers = do
		masterPid <- getSelfPid
		workerPids <- forM workers $ \nid ->
				spawn nid ($(mkClosure ′worker) masterPid) forM_ (zip inputs (cycle workerPids))$
					\(input, workerPid) -> send workerPid input
				r <- collectResults (length inputs)
				liftIO $ print r

To realize these interfaces, we can consider to built on top of Haskell's exising facilities, in ways like:

* _Processes_: use _Concurrent Haskell_'s lightweight threads. The low incremental cost of running threads is important, because a single node may need to support hundreds of processes, and processes may start and end frequently. Lightweight threads could also be used to service network connections; each incoming network connection is handled by forking a new thread.
* _Message port_: Imagine what would happen if messages were received concurrently on two ports and were extracted concurrently by two threads. Only one of the messages can be returned: what is to be done with the other? Haskell’s _Software Transactional Memory (STM)_ helps us solve this problem elegantly. STM provides a mechanism to compose receive transactions on individual message queues in a way that ensures that only one commits.
* _Remote Functions_: _Template Haskell_ could be used, which automatically generate code necessary to invoke remote functions

Apparently, these ideas are not original, but we could make it better in Hackern, since we are building the software stack entirely in Haskell.

### 2.4 File System without POSIX
The another face of our distributed system is a novel file system which is exclusively for Haskell.

Instead of supporting the POSIX APIs for UNIX style file systems, we decide to abandon the POSIX way and use the concepts like the DBFS, or database file system, which support the NoSQL style data accessing.

With NoSQL style data accessing model, we can cope with the scale and agility challenges that face modern applications, esp. in the distributed computing area. So in fact, we are not designing the traditional "file system", but a more general concept of __persistent data storage scheme for distributed usage_.

Considering the problem we are dealing with, we can use key-value style or document-based style. For example, the KV map could be easily manipulated with the Haskell standard library `Data.Map` like:

	phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
	phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs


### 2.5 Network Stack
The network stack is used to support the necessary communication with outside world, or be used as the underlying protocol of our cross-nodes communication.

There is a nice Hackage called "HaNS", which:

> HaNS is a lightweight, pure Haskell network stack that can be used for Haskell networking in the context of the HaLVM, or with a Linux tap device. Currently, HaNS supports 802.3, IPv4, ARP, DHCP (partially), ICMP, UDP, and TCP.

The problem again, is about how to integrate it with Hackern and the distributed computing model.


## 3. Innovations
> In out projects, there are several aspects of innovations, here we will present them to you.

### 3.1 Find a more usable H Interface
The best practice in using Haskell to do low-level operations are still in a state of exploration. We shall combine the idea of H Interface which ease the programming and promise the safety, with the usable aspect of a IaaS service base -- Xen.

The classical Haskell OS, the House, is built towards the real bare metal, but in the scenario of distributed computing, we might actually run the appliance on a heterogenous combination of hardwares, so it would be unrealistic to write driver for each of them.

Instead, we are learning from the concept of VM, and try to build the H interface on top of Hypervisor, which greatly simplifies the process of migrating and adopting.


### 3.2 Security and Verification, for real
In the House project, the verification of Haskell code is stressed and they use a tool called "P-logic" to do this. However, it seems that the DMA part, as well as the assembly and C part are still unsafe.

So, we are planning to do a verification of the low-level C/assembly code, esp. the FFI part.


### 3.3 Haskell Software Stack
If we succeeded in building this system, we are actually introducing many missing part of Haskell Software Stack into the field of a pure Haskell production environment, at least including:

1. HaNS (The Network Stack)
2. Concurrent Haskell
3. NoSQL Library in Haskell, like Persistent or Acid-State.

### 3.4 APIs down to the system-level
In the usual Haskell applications, the APIs are all executed in a user-space, and installed, configured separately.

However, in our scheme of design, the APIs to the underlying system will be executed faster, since it don't have to have some much redundancies. To design such a API layer would require much more inspection and careful coding, which should be worth of a innovative exploration.


Thank you!
