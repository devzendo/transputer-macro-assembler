;
; Copyright (C) 2008-2023 Matt Gumbley, DevZendo.org http://devzendo.org
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; ----------------------------------------------------------------------------
;
; Primary bootstrap routine to be INCLUDEd at the start of binaries that are
; larger than 255 bytes (the maximum size of code that can be loaded via a
; Transputer's built-in boot-from-link mechanism).
;
; The final binary will be larger than 255 bytes - and can be up to 255+64KB.
; The IServer will send all of it down the link. The first byte gives the
; length of this primary bootstrap. This primary bootstrap will then be read
; at MemStart, and executed. The final word Boot2Length (16 bits) at the end
; of this bootstrap declares the size of your larger program. You must declare
; the Boot2End symbol at the end of your code, and this is used to determine
; the size of your code.
;
; This primary bootloader initialises the Transputer, then reads Boot2Length
; bytes from the link starting at the Boot2Start location. After reading the
; rest of your binary, the primary bootloader jumps to Boot2Start.
;
; ----------------------------------------------------------------------------

MemStart        EQU     0x80000070
Boot1Length     EQU     Boot1End - Boot1Start

				ORG     MemStart - 1
				DB      Boot1Length
				ORG     MemStart

Boot1Start:
;
; Workspace layout -----------------------------------------------------------
;
OldIPtr         EQU     6
OldWdesc        EQU     5
LinkInAddress   EQU     4
LinkOutAddress  EQU     3
LoopEnd         EQU     2           ; LoopEnd/LoopValue must be together
LoopValue       EQU     1           ; for the lend instruction to work.
; Workspace[0] cannot be used; outbyte uses it.

;
; Store initial boot state ---------------------------------------------------
; See Compiler Writer's Guide, p74
;
				; Leave room for workspace plus a bit for secondary bootstrap.
				ajw		40	

				; Store old registers.
				stl		OldIptr
				stl		OldWdesc
				stl		LinkInAddress

				; Maybe store the debugging info saved in low memory? See
				; The Transputer Handbook, p44. This is a copy of the high
				; and low IPtr/WPtrs that's stored starting at MemStart.

				; Derive LinkOutAddress
				ldl		LinkInAddress
				ldnlp	-4
				stl		LinkOutAddress

;
; Standard Transputer initialisation -----------------------------------------
;
TptrLoc0        EQU     0x80000024
TptrLoc1        EQU     0x80000028
StartTime       EQU     0

				mint                ; A=NotProcess.p = mint = 0x80000000
				sthf                ; initialize high priority queue front pointer
				mint
				stlf                ; initialize low priority process queue pointer
                ; This must be done before starting any processes or attempting any message passing.
                ; Because of the way queues are implemented there is no need to initialise the back
                ; pointers.
                ; Compiler Writer's Guide p75

				mint
				ldc     TPtrLoc0    ; load high priority timer queue pointer offset
				stnl    0           ; initialize high priority timer queue
				mint
				ldc     TPtrLoc1    ; load low priority timer queue pointer offset
				stnl    0           ; initialize high priority timer queue
                ; This must be done before any attempt to wait on the timer. Also before using the
                ; clock at any priority level, the timer must be started...

				ldc     StartTime   ; load time to initialize clocks at (usually zero)
				sttimer             ; start the clocks

				testerr             ; clears the Error flag and HaltOnError flag
				clrhalterr          ; or use sethalterr here depending what you want

				fptesterr           ; Reset floating point error flag and set rounding mode to Round-to-Nearest

				mint                ; store NotProcess.p to Event channel
				ldc     0x80000020
				stnl    0
				mint                ; store NotProcess.p to Link 3 input channel
				ldc     0x8000001C
				stnl    0
				mint                ; store NotProcess.p to Link 2 input channel
				ldc     0x80000018
				stnl    0
				mint                ; store NotProcess.p to Link 1 input channel
				ldc     0x80000014
				stnl    0
				mint                ; store NotProcess.p to Link 0 input channel
				ldc     0x80000010
				stnl    0
				mint                ; store NotProcess.p to Link 3 output channel
				ldc     0x8000000C
				stnl    0
				mint                ; store NotProcess.p to Link 2 output channel
				ldc     0x80000008
				stnl    0
				mint                ; store NotProcess.p to Link 1 output channel
				ldc     0x80000004
				stnl    0
				mint                ; store NotProcess.p to Link 0 output channel
				ldc     0x80000000
				stnl    0

;
; Read the secondary bootstrap from the link ---------------------------------
;

				; Read in the code.
				; The number of bytes is stored at Boot2Length.
				ldc		Boot2Start
				ldl		LinkInAddress
				ldc		Boot2Length - ll3
				ldpi
ll3:			ldnl    0					; (a=length, b=channel, c=address)
				in

				; Execute it.
				ldc		Boot2Start
				gcall

				ALIGN

Boot2Length:    DW  Boot2End - Boot2Start
Boot1End:

Boot2Start:
; NB: Your code must define Boot2End at its end.
