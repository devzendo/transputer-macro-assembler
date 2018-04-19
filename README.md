Transputer Macro Assembler
==========================
This is a simple macro assembler for Transputer assembly language, that can
build binary files. The future intention is to also build ELF object files.

The assembler is to be used in the building of the
[transputer-eforth](https://bitbucket.org/devzendo/transputer-eforth) eForth
port, and also as the assembler used by the [Parachute
Project](https://devzendo.github.io/parachute) and its future languages.

Rationale
---------
I needed a macro assembler to assemble eForth. I have MASM 6, which works fine,
but is closed source, and in 2018, quite difficult to obtain legitimately. (I
have Windows XP SP2, Visual C++ Studio 6.0, and the Processor Pack 5 - this
latter is now unobtainium, so was retreived via the Wayback Machine - more
recent versions of Windows, e.g. 7, do not install Visual Studio correctly)

Asking contributors to an open source project to buy copies of old commercial
projects will be a blocker to contribution, and is against the open source
ethos. 

MASM 6 is a fine product; I wish Microsoft would make old systems freely available
via some legitimate archive... but this is not likely.

I tried assembling it with JWasm, the open source version of the Watcom
assembler, but this had issues with relocation and endianness. Yes, I could have
investigated this and submitted patches... but what is required for this project
is not complex, and does not need an x86 assembler just acting as a macro
assembler for DB directives.

I tried porting it to NASM, the netwide assembler, but could not get the macro
system to work as documentation suggests.

I have previously tried to get the ttools assembler/linker building, but this
has issues on 64-bit systems. See 
[transputer-toolchain](https://bitbucket.org/devzendo/transputer-toolchain) for
this abandoned project.


Syntax
------
Since the initial project using this assembler is eForth, and eForth has
traditionally been written using Microsoft Macro Assembler 5.1, the syntax
follows that assembler. Only a small subset of MASM's language is needed.

The following fragment illustrates most of the key syntax:
```
```

Getting Started
----------------
The assembler requires Java 8 (it's written in Scala), with the 'java' JRE
runtime available on your PATH. For convenience, add the 'bin' directory of the
assembler's distribution to your PATH.

The assembler is invoked via the 'tmasm' command. Running 'tmasm -h' gives a
full summary of command line arguments.

For example, to assemble the file test.asm, producing a binary file and listing
file:

```
tmasm -i test.asm -o test.bin -l test.lst
```

Contributing
------------
The assembler is written in Scala, and requires Java 8. The build system is
Maven. I use Scala's Parser Combinators. I use TDD rigourously, and require
contributions to keep code coverage/testability as high as possible. 

All contributions are welcome!

Contact me, Matt Gumbley via matt.gumbley@devzendo.org, or @mattgumbley on
Twitter.


License
-------
This code is released under the Apache 2.0 License: http://www.apache.org/licenses/LICENSE-2.0.html.
(C) 2018 Matt Gumbley, DevZendo.org

