Elementary Macro Assembler with Transputer Support
==================================================
This is a simple byte-building macro assembler, that can build binary files,
irrespective of actual CPU instructions. You can build your set of 'opcodes'
via macros and ALIGN/DB/DD/DW directives.

It also understands variants of the Transputer assembly language when the CPU
directive selects a Transputer variant. Initially targetting the T800.

The future intention is to also build ELF object files.

The assembler is to be used in the building of the
[transputer-eforth](https://bitbucket.org/devzendo/transputer-eforth) eForth
port, and also as the assembler used by the [Parachute
Project](https://devzendo.github.io/parachute) and its future languages.

Status
------
In development; unfinished; not yet at its first release (as of April/May 2018).

Currently working on the parser, and macro expansion. No code generation yet.

Remaining work:

* got argument and parameter wrong, d'oh
* is this word a macro?
* macro invocation, splitting of non-space things, possibly into brackets, into macro params
* macro invocation with params split on space
* macro expansion via the macro manager
* nested macro invocation (e.g. $COLON uses $CODE)
* handling conversion of exceptions that the macro manager might throw when expanding
* label on macro invocation must appear on first line of expansion only
* calling back into the parser to convert expanded strings into Lines, flatmap these into the macro expansion
  output, retaining the initial macro invocation's single line number
* better case insensitivity of keywords https://stackoverflow.com/questions/38070885/how-to-handle-case-insensitive-keywords-with-scala-parser-combinators
* macros shouldn't be able to replace keywords
* directives: if1/endif, dup (n) after db/dw/dd, end.* (ignore)
* honour the Cpu directive, to switch in Transputer family native instructions, rather than db-generating macros
* Transputer pfix/nfix generation from direct opcodes
* documentation of syntax
* code generation
  * prevent reassignment to the same constant
  * handle overflowing DB/DD/DW data in code generation

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

