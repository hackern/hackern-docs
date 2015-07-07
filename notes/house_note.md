concurrent and the use of QSem

### cbits
The `cbits` directory contains a dozen of c source/header, providing IA32 privilege operation.
Many of these functions are just wrapper of a single line assembly, and will be called from H using FFI.

### H
+ Pages.hs
  + allocPage
    + Used by `PhysicalMemory.hs` and `VirtualMemory.hs`

+ PhysicalMemory.hs
  + allocPhysPage
    + Automatically freed through Haskell weakRef.

+ VirtualMemory.hs
  + allocPageMap
    + Initial page map are just copy of the kernel page map, but the first 64MB is inaccessible in user-mode.
    + The first 64MB are mapped to indentical physical pages.
    + Allowing kernel to access physical memory without changing page directory.

  + freePageMap
    + Automatically took care by Haskell weakRef

  + setPage pm vaddr (Just pinfo)
    + Change the entry in page table in page directory according to vaddr.
    + Address starting from 256MB are mapped for user processes.

  + getPage

+ UserMode.hs
