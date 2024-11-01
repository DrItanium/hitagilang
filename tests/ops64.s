	.text
	.align 4
	.globl _addol
	#  Function 'addol'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_addol:
	movl	g0,g4 # m4
	addo	g0,g2,g0
	cmpo	g0,g4
	testl	g6
	addo	g5,g3,g1
	addo	g6,g1,g1
.Li960R1:	ret
	.align 4
	.globl _subol
	#  Function 'subol'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_subol:
	movl	g0,g4 # m4
	subo	g2,g0,g0
	cmpo	g0,g4
	testg	g6
	subo	g3,g5,g1
	subo	g6,g1,g1
.Li960R2:	ret
	.align 4
	.globl _mulol
	#  Function 'mulol'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp 
_mulol:
	movl	g0,g4 # m4
	emul	g0,g2,g0
	mulo	g4,g3,g6
	addo	g1,g6,g6
	mulo	g2,g5,g2
	addo	g6,g2,g1
.Li960R3:	ret
	.align 4
	.globl _divol
	#  Function 'divol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_divol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___udivdi3
.Li960R4:	ret
	.align 4
	.globl _remol
	#  Function 'remol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_remol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___umoddi3
.Li960R5:	ret
	.align 4
	.globl _shlol
	#  Function 'shlol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shlol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___ashldi3
.Li960R6:	ret
	.align 4
	.globl _shrol
	#  Function 'shrol'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shrol:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___lshrdi3
.Li960R7:	ret
	.align 4
	.globl _andl
	#  Function 'andl'
	#  Registers used: g0 g1 g2 g3 fp 
_andl:
	and	g0,g2,g0
	and	g1,g3,g1
.Li960R8:	ret
	.align 4
	.globl _orl
	#  Function 'orl'
	#  Registers used: g0 g1 g2 g3 fp 
_orl:
	or	g0,g2,g0
	or	g1,g3,g1
.Li960R9:	ret
	.align 4
	.globl _notl
	#  Function 'notl'
	#  Registers used: g0 g1 fp 
_notl:
	not	g0,g0
	not	g1,g1
.Li960R10:	ret
	.align 4
	.globl _xorl
	#  Function 'xorl'
	#  Registers used: g0 g1 g2 g3 fp 
_xorl:
	xor	g0,g2,g0
	xor	g1,g3,g1
.Li960R11:	ret
	.align 4
	.globl _xnorl
	#  Function 'xnorl'
	#  Registers used: g0 g1 g2 g3 fp 
_xnorl:
	xor	g2,g0,g2
	xor	g3,g1,g3
	not	g2,g2
	not	g3,g3
	movl	g2,g0 # m4
.Li960R12:	ret
	.align 4
	.globl _nandl
	#  Function 'nandl'
	#  Registers used: g0 g1 g2 g3 fp 
_nandl:
	and	g2,g0,g2
	and	g3,g1,g3
	not	g2,g2
	not	g3,g3
	movl	g2,g0 # m4
.Li960R13:	ret
	.align 4
	.globl _norl
	#  Function 'norl'
	#  Registers used: g0 g1 g2 g3 fp 
_norl:
	or	g2,g0,g2
	or	g3,g1,g3
	not	g2,g2
	not	g3,g3
	movl	g2,g0 # m4
.Li960R14:	ret
	.align 4
	.globl _addil
	#  Function 'addil'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_addil:
	movl	g0,g4 # m4
	addo	g0,g2,g0
	cmpo	g0,g4
	testl	g6
	addo	g5,g3,g1
	addo	g6,g1,g1
.Li960R15:	ret
	.align 4
	.globl _subil
	#  Function 'subil'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp cc 
	#		   
_subil:
	movl	g0,g4 # m4
	subo	g2,g0,g0
	cmpo	g0,g4
	testg	g6
	subo	g3,g5,g1
	subo	g6,g1,g1
.Li960R16:	ret
	.align 4
	.globl _mulil
	#  Function 'mulil'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 fp 
_mulil:
	movl	g0,g4 # m4
	emul	g0,g2,g0
	mulo	g4,g3,g6
	addo	g1,g6,g6
	mulo	g2,g5,g2
	addo	g6,g2,g1
.Li960R17:	ret
	.align 4
	.globl _divil
	#  Function 'divil'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_divil:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___divdi3
.Li960R18:	ret
	.align 4
	.globl _remil
	#  Function 'remil'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_remil:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___moddi3
.Li960R19:	ret
	.align 4
	.globl _shlil
	#  Function 'shlil'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shlil:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___ashldi3
.Li960R20:	ret
	.align 4
	.globl _shril
	#  Function 'shril'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
_shril:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#End Prologue#
	callx	___ashrdi3
.Li960R21:	ret
