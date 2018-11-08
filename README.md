transputer-macro-assembler
==========================
This is a simple byte-building macro assembler, that can build binary files,
irrespective of actual CPU instructions. You can build your set of 'opcodes'
via macros and ALIGN/DB/DD/DW directives. Its input language is a small subset
of Microsoft MASM 5.1.

It also supports the instruction set of the Inmos T414/T800/T801/T805 Transputer,
when the .TRANSPUTER directive is given.

(C) 2018 Matt J. Gumbley
matt.gumbley@devzendo.org
@mattgumbley @devzendo
http://devzendo.github.io/parachute

The assembler is to be used in the building of the
[transputer-eforth](https://bitbucket.org/devzendo/transputer-eforth) eForth
port, and also as the assembler used by the [Parachute
Project](https://devzendo.github.io/parachute) and its future languages.

Status
------
Project started 19 April 2018.
Successful eForth assembly verified 25 June 2018.
 
In active development; unfinished; not yet at its first release (as of November 2018).

Parser, macro expansion, code generation and output of binary file and listing are done. Optimally encodes direct
instructions into pfix/nfix sequences - especially for forward references where location might not yet be known.

The assembler has built a binary of eForth without any parsing/code gen errors (using macros, not .TRANSPUTER mode yet)
- and a close inspection of the listing side-by-side with that produced by MASM suggests that I'm assembling identically
to MASM! I can't generate a binary with MASM - linking fails - but the listing is sufficient for verification.

Current work:
  * Investigate/bugfix: Constants/Variables definition in listing only shows 16-bit versions of values?
      
Remaining work:
  * Escape codes do not work in DB strings? \12 ? \n ? \r ?
  * local labels
  * global and extern symbols
  * INCLUDE, in lieu of building linkable object files
  * Add builds/install scripts for Linux and Windows

* later....
  * ELF writing (or other object format: TCOFF?)  

* nice-to-haves...
  * binary map listing, showing symbols sorted by address and the binary dump at those addresses.
  * handling conversion of exceptions that the macro manager might throw when expanding
  * macros shouldn't be able to replace keywords
  * exception handling - is... odd..... use Try instead of throwing? Easier collection of phase errors?
  * There's duplication in the storage of references to storage contents/symbols so that if embedded constants/labels
    change during convergence, the storage/expressions are re-evaluated; should unify this and use closures to
    process fixups.  

Release Notes
-------------
0.01 (ongoing work for the first release)
* Added an OFFSET operator, to compute offsets of its argument from $.
* Added support for the full T414/T800/T801/T805 instruction set in .TRANSPUTER mode.
* Added the T801 instructions, from "Transputer Instruction Set - Appendix, Guy Harriman".
* Added the T801/T805 instructions from "Support for debugging/breakpointing in transputers" (INMOS
  Technical Note 61).
* Added the Parachute Transputer Emulator's nonstandard instructions.

Rationale
---------
I needed a macro assembler to assemble eForth (and in a reusable form for future Transputer language projects). I have
MASM 6, which works fine, but is closed source, and in 2018, quite difficult to obtain legitimately. I have Windows XP
SP2, Visual C++ Studio 6.0, and the Processor Pack 5 (which contains MASM) - this latter is now considered
'unobtainium' from Microsoft , so was retreived via the Wayback Machine - more recent versions of Windows, e.g. 7, do
not install Visual Studio correctly. It is possible to install it in XP SP2 then copy the executables across to Windows
7, which is what I did since XP is not registerable any more, apparently.

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
accepted by this assembler is the very small subset of MASM need by eForth.

For identifier syntax, see [The Java Language Spec](http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.8)

The following fragment illustrates most of the key syntax:
```
LABEL:    ; A label starting a line with a colon; a semicolon introduces a comment.
          EQU MYCONST 42 ; Here's a constant; you can't re-assign them (see Convergence).
          EQU X 2 OR 7   ; Can use expressions: OR ||, AND &&, +, -, *, /, SHR >>, SHL <<,
                         ; ! ~ (unary negation/complement)  
          MYVAR = 69     ; Here's a variable; you can re-assign them.
                         ; Constant, variable, and label names follow the rules for
                         ; Java identifiers: this means a letter, currency symbol or connecting
                         ; punctuation symbol (_), followed by zero or more letters or numbers.
          ORG 0x40000000 ; Set the memory location to assemble at ($) to this address,
                         ; here given in hex. The default for the $ variable is 0.
          DB 1,2,3,'hi'  ; Assemble three bytes and a string at $ (0x40000000).
                         ; Arguments to data assemblers can be decimal numbers (e.g. 14),
                         ; hexadecimal numbers (e.g. 0x20, 20H), labels, constants,
                         ; variables, or supports single- or double-quoted strings
                         ; of characters. Values are limited to unsigned 0-255.
          DW 0x1234      ; Assemble one or more words, separated by commas. Can be decimal
                         ; or hex as per DB. Range 0-65535. 
          DD 0xAA55AA55  ; Assemble one or more double words, separated by commas. Decimal
                         ; or hex, range 0-4294967295.
          DB 5 DUP 7     ; Assemble 5 instances of the byte 7. Same as DB 7,7,7,7,7. Can
                         ; also use DW count DUP n, and DD count DUP n.
          TITLE My Title ; Sets the title given in the listing.
          PAGE 80,25     ; Sets the page size in the listing.
          .386           ; Sets the instruction set to 386 and endianness to Little.
                         ; (If you want to write Transputer code in .386 mode, note that this only sets the endianness
                         ;  you have to build macros up to generate DBs from opcodes - as eForth originally does.)
          .TRANSPUTER    ; Sets the instruction set to T414/T800/T801/T805 and endianness to Little.
                         ; Note that by default, the assembler's endianness is Big.
          ALIGN 4        ; Increase $ if necessary to the next address that's a multiple
                         ; of the argument.
          IF1            ; Mark out a section that will only be assembled during pass 1.
          ELSE           ; Mark out a section that will only be assembled during pass 2.
          ENDIF          ; End the pass1/pass2 section.
          END            ; Marks the end of the source file. Must be present; no statements
                         ; are allowed after this.
          name MACRO param1,param2,param3...
                         ; Start definition of a macro with name 'name', and the list of
                         ; named parameters. Can supply zero or more parameters.
                         ; The parameters can be referred to in the body of the macro,
                         ; which follows....
          ENDM           ; End macro definition; must follow MACRO.
          name 1,2,3     ; Invoke macro called 'name', substituting 1 for param1, 2 for param2, 3 for param 3.
          MAIN           ; Ignored
          ASSUME         ; Ignored
          .LIST          ; Ignored
          .NOLIST        ; Ignored        
          MYOFF = OFFSET MYLABEL
                         ; Sets the MYOFF constant to the difference between MYLABEL and $, which is positive if
                         ; MYLABEL is ahead of this statement, negative if it is behind. OFFSET can be used in any
                         ; expression: in an ORG (odd, but possible!), DB/DW/DD and their DUP forms, in constant (EQU)
                         ; and variable (=) definitions, and in Transputer direct instruction arguments, e.g.
                         ; LDC OFFSET MYLABEL. This is especially useful for CJ/J/CALL instructions which work with
                         ; offsets to instructions rather than addresses - to allow code to be position-independent.
                         ; OFFSETs are also recomputed during convergence (see below).
```

Convergence
-----------
The transputer instruction set encodes 15 common instructions as 'Direct Instructions' in a single byte. The most
significant nybble is the opcode; the least significant nybble is the operand, which can range from 0-15. For operands
outside this range, the pfix/nfix direct instructions are used to precede the direct instruction being built, such that
the full range of 32-bit operands can be used in these common instructions.

This means that direct instructions can sometimes require a variable-length encoding. Where operands are forward
references to addresses not yet known, an iterative approach is taken to build up these encodings - thereby changing the
addresses of intervening symbols, until a stable set of encodings is available. This results in the most compact code,
and is guaranteed to converge - if a little slowly.

When the address of a label/constant/variable changes due to convergence, all references to it are re-evaluated.
Even if a constant is defined to refer to a changing address.

Using the Assembler
-------------------
The assembler requires Java 8, with the 'java' JRE runtime available on your PATH.
For convenience, add the 'bin' directory of the assembler's distribution to your PATH.

Distributions of the Parachute project will include the assembler in its bin/lib directories.

The assembler is invoked via the 'tmasm' command. Running 'tmasm -h' gives a
full summary of command line arguments.

For example, to assemble the file test.asm, producing a binary file and listing
file:

```
tmasm -b test.bin -l test.lst test.asm 
```

By default the assembler is case-insensitive, like MASM's /Cu option. All variables,
constants and labels are converted to upper case. To switch to processing variables,
constants and labels in a case-sensitive manner, use the -x (--caseSensitive) option.
This is like MASM's /Cx option.


Contributing
------------
The assembler is written in Scala, and requires Java 8. The build system is
Maven. I use Scala's Parser Combinators. I use TDD rigourously, and require
contributions to keep code coverage/testability as high as possible. 

All contributions are welcome!

Contact me, Matt Gumbley via matt.gumbley@devzendo.org, or @mattgumbley on
Twitter.


Building
--------
Just requires Java 8 and Maven 3.2.x.

'mvn clean package' is all you need.


License
-------
This code is released under the Apache 2.0 License: http://www.apache.org/licenses/LICENSE-2.0.html.
(C) 2018 Matt Gumbley, DevZendo.org


Acknowledgements
----------------
Christoph Henkelmann's excellent series on Parser Combinators:
https://henkelmann.eu/tags/parser-combinators/

Including his Java expression parser:
https://github.com/chenkelmann/parser_example/blob/master/src/main/scala/eu/henkelmann/parser02/ExpressionParsers.scala


Thanks to Andrey Tyukin for assistance in composing parser combinators as traits:
https://stackoverflow.com/questions/51199613/composing-scala-parser-combinators-at-runtime
