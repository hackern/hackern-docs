# Build a House from scratch -- Some ideas and questions

## From the house's codebase
If we want to build a minimal from the house's codebase, we might have to:

1. Extract the booting related code, write a `main` in C to test it
2. Rebuild the minimal libc for modern Haskell RTS, build RTS
3. Try to compile a minimal Haskell code with RTS
4. Link Haskell `main`, RTS, libc and bootstrapping code into a image file

## Some general ideas:

1. Boot (C/assembly)
    + Grub -- stage2, Multiboot
    + Real mode -- House seems working only in protected mode
    + Kernel Loader -- When and How load Kernel? With RTS?
    + Basic Initialization -- Global Tables, Segmentation, Timer etc.
2. IDT/GDT etc
    + Service Defination -- Interrupt Table, Handler in what?
    + Data Structure -- In what language, C, assembly or Haskell?
    + Initialization -- What? When?
3. I/O Port
    + Memory Mapped I/O -- Direct Writing? Specialized Interface?
    + Foreign Pointer -- header file
4. Basic Memory/Process Environment
    + Memory layout -- Haskell Heap and C Heap?
    + cr3 register -- Interface?
5. mini libc + RTS
    + Selection and Compilation -- What libc component did House use? Hwo to use
    + Port the RTS: Difficult part
6. The control transfer
    + Invoke Haskell Kernel -- How?
    + What is transferred, seriously?
7. H Interface
    + I/O -- Text mode IO handlers?
    + Pointer -- Wrap c pointer, or address, into what?
    + Task -- How to represent a "task" in memory?
    + Page -- How to wrap a page? What is the basic operations?
    + Other hardware interfaces -- Timer, Interrupt handler, etc.
8. Kernel program in Haskell
    + Process Scheduler: Context, Process Create/Switch/Termination
    + Memory Virtualization: Paging, Directory etc.
9. System interface + User mode
    + System Library -- What is offered? The relationship with RTS? Haskell module? POSIX?
    + User mode execution
10. Testing
    + Potential Bug?
    + Performance?
    + Resource Utilization?
11. User-defined Binary Execution
    + Loader -- How to load? Is binary layout different from EFI?
    + Special Complilation options?
12. Review
    + House and L4
    + Verve
    + Mirage OS
    + Kinect