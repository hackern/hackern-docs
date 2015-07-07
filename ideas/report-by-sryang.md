### 操作系统的脆弱健壮与否与编程语言没有直接关系。比如unix/linux的实现
操作系统也是软件的一种，操作系统也会出Bug，也会被Attack。Unix/Linux开发的时候编程语言并没有发展到现在的水平，C/C++是当时最好的选择。Linux背后有遍布全球的开源团队在维护。

### Haskell为什么没有流传开来？不能只看优点，其缺点分析没有给出，调研不够充分
小众。Non-strict。Haskell的模式不一定是Best，但至少是Interest。

### 并行/分布式在目前的平台上也得到了很好的实现，为什么一定要用Haskell
两方面看：

1. 实现Haskell技术栈；
2. Haskell语言自身的优势，目前的不少分布式平台也采用Scala，Go等现代的编程语言实现。

### 给出的更多的是编程语言的优点，但和操作系统的关系有多大？所提的很多优点其实在应用层已经有很好的实现，那么为什么要做到os层面，其意义何在？
OS的角色和架构正在改变。例子包括诸多Hypervisor，运行在硬件上的JVM，Unikernels这样的Library OS，等等。在服务端，OS的角色正逐渐从隔离multiple users，变为for single purpose。
Language VM (runtime), OS, Hypervisor均为上层的App提供了抽象和保护，他们中的功能存在着一定的交集。
像Unikernels中，OS可以以Library的形式链接到App当中，内核层用OCaml实现了众多的Type-Safe Protocol I/O，integrate well with上层的OCaml App，性能得到了一定的提升，同时也保证了底层的安全性。

### 为什么是Haskell，而不是其它高级语言
静态类型，强类型，编译。具备这几个条件的基本只剩下Haskell，OCaml，Golang，Rust。前两者发展历史比较长，后两者有好爸。

#### 你们对于Haskell的掌握程度
#### 分布式计算并不难实现，跟操作系统有什么关系？为什么用操作系统？
#### 做成什么样的操作系统没有说清楚
