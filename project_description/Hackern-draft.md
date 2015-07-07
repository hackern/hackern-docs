# [Draft] A highly-reliable, lightweight and domain-specific kernel based on functional language Haskell

## Highlights
* Rapid in Development
* Reliable in Production
* Pioneering in Research
* Performance in I/O
* Natural in Parallelism

## Foundation
* House: The Operating System Prototype in Haskell 
* The Singularity Project: Exploring new ways of development certified kernel in high-level language
* Distributed-haskell: A distributed computing library in Haskell
* Atom: Using Haskell to develop real-time, embedded software

## Why we should use Haskell to develop a kernel?
* C is error-prone, weak-typed, boundary-unchecked, syntactically poor and with serious side-effects
* GHC compiler can compile highly optimized code, with nearly C’s performance in certain aspects. What’s more, there is also JHC compiler, which aims at even more efficient code for system-level program.
* Compared to other functional language, Haskell has many system-programming constructs which could generate very low-level, compiled code, which maintaining Haskell’s advanced characteristics in source code.
* Haskell is strong-typed, statically type-checked, and with excellent separation with IO-related code and purely functional code, which promises a high-level of code reliability.
* Haskell is lazy-evaluated generally, so we could use this feature to naturally write I/O efficient code. However, Haskell also support strict evaluation when needed by some special, critical mission which is common in OS.
* Haskell is very expressive and easy to write, so the development will be much faster and the code will be easier to maintain.
* Functional programming is parallel in nature, so it is easier to develop some concurrent/parallel computing facilities.

## Our Objects
* Develop a light-weight, domain-specific (like embedded computing, distributed computing or real-time control) kernel written in Haskell, based on the previous general-purpose, prototype-level kernels.
* [Optional] Use some checker or proof assistant to verify the mixed C/assembly part
* [Optional] Design a more human-friendly, more secure shell interface
* [Optional] For distributed computing as an example, we will try to write some POSIX-compatible, easy-to-use framework based on the bare kernel

## Our Schedule
* Determine the final project specification, while analyze and study the previous prototypes, such as “House” and “Atom” etc. Estimated time: 1 month
* Start to write the kernel based on the previous prototype, but more efficient and light-weight in certain domain application. Estimated time: 1 month
* Build/Test the kernel, while select some optional features to add on top of the kernel. Estimated Time: 1 month
* Profiling & optimize the code under some application-context testings. Write document and do some other cleanups.

## The (Primitive) Feasibility Analysis
* All of the three student members (Yang Siran, Yin Yu, Zhang Zhen) have already learned Haskell language before this semester, and all with functional language project experience before.
* We are all familiar with the system-level knowledge as well as Linux/UNIX OS and have some development experience.
* Yin Yu has some Spark development experience, Siran Yang has investigated into some distributed computing OS before (like Inferno), while Zhen Zhang know some heterogenous parallel computing, like CUDA programming.
* Project-specific feasibility is already cover in “Why we should use Haskell to develop a kernel?”

## The originality, creativity and application analysis
* Originality: Nearly all previous kernel prototypes are not covering some real world application but mostly experimental and under development. So  out improvements should be original and unique.
* Creativity: This project is trying to find a new way of developing system software, by use of high-level, functional and strong-typed language. Success or not, the exploring itself should be a highly creative activity and even deserve some research value.
* Application: From the start of our discussion, we are highly focused on its real world application, such as distributed computing. So we are not just playing a toy, but seriously trying to make it work like a horse. What’s more, compared to today’s OS on market, large portion of them are very unstable and poorly verified if written in C, while our kernel, if working, will be very reliable, robust and secure.