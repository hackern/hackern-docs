HaLVM Build Problems
===

## The basic process and some notices
I built HaLVM from source on GitHub.

Although the source is not large, but the `Makefile` will download **a bunch of stuff** from Internet. So please prepare a non-GFW networking environment before compiling.

The Galois Inc. says they tested on 64-bit, Xen 4.2, Fedora 19-based systems, but also:

> it works on my machine, sometimes, and I'm hoping it works on yours, sometimes.

I compiled it on my fedora VM and the only compilation problem I encountered should be:

* `llvm` (it seems not a problem, I found it when I recompiled again)
* `openssl-devel`
* `cabal` (fedora's `yum` don't have it in source, so install `haskell-platform` might be a choice)

And a final thing to remember: the `/tmp` on my fedora box is only 1.1G initially, which will be filled entirely at the very first building process (that might be a bug). So, you might needs to enlarge it before continuing.

Good luck.

## The latest problem I asked on mailist: The mail I sent

Hi, everyone!

I am Zhen Zhang and I just complied and installed HaLVM on my Fedora Linux VM on my OS X laptop.

The compilation and installation seems to be completed without fatal errors, but the running seems having big problem.

If I use “sudo mkrenddir”, the echo would be:

    sudo: mkrenddir: command not found

So I directly use `/usr/local/bin/mkrenddir` and directly `sudo` this, the echo would be like:

    Could not open XenStore. You may need to add ‘sudo’.

When I changed to root and `mkrenddir`, the echo is same as the above one.

From the source file, I identified this:

	if(!xsd) {
		printf(“Could not open XenStore. You may need to add ‘sudo’.\n”);
		return 1;
	}

I am not familiar with Xen and only searched something saying that  xs_open  is used to “Open a connection to the xs daemon.”

So, is it possible that the Xen installation has some problems?

Is there anyone who has encountered similar problem before?


## Current Status

After deploying Xen on my fedora host, now I can use `xenstore` to launch it.

To test it, you must first setup the VM, using `mkrenddir` command, then find some example **code and config** to play with.

Now, I have successfully completed the program `Time`, but the

* `Hello`
* `DomainInfo`

have failed.

For the `Hello`, it used a `printDebugInfo` which needs some customized compilation. For the other one, it is also the problem with IO I suppose.