Elementary Macro Assembler with Transputer Support
==================================================
This is a simple byte-building macro assembler, that can build binary files,
irrespective of actual CPU instructions. You can build your set of 'opcodes'
via macros and ALIGN/DB/DD/DW directives. Its input language is a small subset
of Microsoft MASM 5.1.

It also understands variants of the Transputer assembly language when the CPU
directive selects a Transputer variant. Initially targetting the T800.

The future intention is to also build ELF object files.

The assembler is to be used in the building of the
[transputer-eforth](https://bitbucket.org/devzendo/transputer-eforth) eForth
port, and also as the assembler used by the [Parachute
Project](https://devzendo.github.io/parachute) and its future languages.

Status
------
Project started 19 April 2018.
In development; unfinished; not yet at its first release (as of mid-June 2018).

Parser, macro expansion, code generation and output of
binary file and listing are done.

Remaining work:
* bugs & limitations...
  * it should be possible to set constants to as-yet-undefined values e.g. temp1
  * dist equ (rx1-from1)
  * in macrolabel.asm, is the _CODE-_LEN-1 evaluated correctly (it gives 7; should it be 6?)
* nice-to-haves...
  * handling conversion of exceptions that the macro manager might throw when expanding
  * macros shouldn't be able to replace keywords
  * exception handling - is... odd..... use Try instead of throwing? Easier collection of phase errors?

* later....
  * honour the Cpu directive, to switch in Transputer family native instructions, rather than db-generating macros
  * Transputer pfix/nfix generation from direct opcodes
  * ELF writing
  
Rationale
---------
I needed a macro assembler to assemble eForth (and in a reusable form for future
Transputer language projects). I have MASM 6, which works fine,
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
accepted by this assembler is the very small subset of MASM need by eForth.

For identifier syntax, see [The Java Language Spec](http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.8)

The following fragment illustrates most of the key syntax:
```
LABEL:    ; A label starting a line with a colon; a semicolon introduces a comment.
          EQU MYCONST 42 ; Here's a constant; you can't re-assign them.
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
          .386           ; Sets the instruction set and endianness to 386.
          .T800          ; Sets the instruction set and endianness to T800.
                         ; (Currently only the endianness is set; build macros up
                         ; to generate DBs from opcodes)
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

