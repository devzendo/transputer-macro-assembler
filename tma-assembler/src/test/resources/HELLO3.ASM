	.TRANSPUTER
	MemStart    EQU 0x80000070
	MemLength   EQU MemStop - MemStart
	ORG         MemStart - 1
	DB          MemLength
	ORG         MemStart
	j       MAIN
	REQ_PUTS        EQU 0x0f
	STDOUT_STREAMID EQU 0x00
	LINK0_OUTPUT    EQU 0x80000000
	LINK0_INPUT     EQU 0x80000010
HWSTR:
	DB      "hello world", 0x00 ; length 12
MAIN:
	ajw     0x100    ; allow for 64 stack frames
	ldc     HWSTR - _M1
	ldpi
_M1:
	call    putConsolePString
	terminate
putConsolePString:
	PCPS_WLEN       EQU 2
	PPS_STRINGADDR  EQU 0
	PPS_STRINGLEN   EQU 1
	ajw     -PCPS_WLEN
	WS_STR_ADDR EQU 0x03
	ldl     WS_STR_ADDR
	dup
	stl     PPS_STRINGADDR
	call    strlen
	dup
	stl     PPS_STRINGLEN
	adc     9
	call    outshort0
	ldc     LINK0_OUTPUT
	ldc     REQ_PUTS
	outbyte
	ldc     LINK0_OUTPUT
	ldc     STDOUT_STREAMID
	outword
	ldl     PPS_STRINGLEN
	call    outshort0
	ldl     PPS_STRINGADDR
	ldc     LINK0_OUTPUT
	ldl     PPS_STRINGLEN
	out
	ajw     PCPS_WLEN
	ret

outshort0:
	WSOS0_WORD EQU 0x01
	ldc     LINK0_OUTPUT
	ldlp    WSOS0_WORD
	rev
	ldc     2
	out
	ret

strlen:
	ldl     0x01
	ldc     0
_sl_loop:
	rev
	dup
	lb
	cj      _sl_end
	adc     1
	rev
	adc     1
	j       _sl_loop
_sl_end:
	pop
	pop
	ret

MemStop:
	END
