	.text
	.align 4
	.globl __Z13extractLowesti
	#  Function '_Z13extractLowesti'
	#  Registers used: g0 g1 g2 fp sp 
	#		   r3* 
__Z13extractLowesti:
	subo	1,0,g1	# ldconst -1,g1 # ldconst 40
	mov	0,g2
	callx	__Z12extractValueIicET_T0_S1_S1_
	shlo	24,g0,g0
	shro	24,g0,g0
.Li960R1:	ret
	.align 4
	.globl __Z12extractValueIicET_T0_S1_S1_
	#  Function '_Z12extractValueIicET_T0_S1_S1_'
	#  Registers used: g0 g1 g2 fp 
__Z12extractValueIicET_T0_S1_S1_:
	shlo	24,g0,g0
	shro	24,g0,g0
	shlo	24,g2,g2
	shro	24,g2,g2
	shri	g2,g0,g0
	shlo	24,g1,g1
	shro	24,g1,g1
	and	g0,g1,g0
.Li960R2:	ret
	.align 4
	.globl __Z12extractLoweri
	#  Function '_Z12extractLoweri'
	#  Registers used: g0 g1 g2 fp sp 
	#		   r3* 
__Z12extractLoweri:
	subo	1,0,g1	# ldconst -1,g1 # ldconst 40
	mov	8,g2
	callx	__Z12extractValueIicET_T0_S1_S1_
	shlo	24,g0,g0
	shro	24,g0,g0
.Li960R3:	ret
	.align 4
	.globl __Z13extractHigheri
	#  Function '_Z13extractHigheri'
	#  Registers used: g0 g1 g2 fp sp 
	#		   r3* 
__Z13extractHigheri:
	subo	1,0,g1	# ldconst -1,g1 # ldconst 40
	mov	16,g2
	callx	__Z12extractValueIicET_T0_S1_S1_
	shlo	24,g0,g0
	shro	24,g0,g0
.Li960R4:	ret
	.align 4
	.globl __Z14extractHighesti
	#  Function '_Z14extractHighesti'
	#  Registers used: g0 g1 g2 fp sp 
	#		   r3* 
__Z14extractHighesti:
	subo	1,0,g1	# ldconst -1,g1 # ldconst 40
	mov	24,g2
	callx	__Z12extractValueIicET_T0_S1_S1_
	shlo	24,g0,g0
	shro	24,g0,g0
.Li960R5:	ret
	.align 4
	.globl __Z14extractSrcDesti
	#  Function '_Z14extractSrcDesti'
	#  Registers used: g0 fp sp 
	#		   r3* 
__Z14extractSrcDesti:
	callx	__Z12extractValueIhiLi31ELi19EET_T0_
	shlo	24,g0,g0
	shro	24,g0,g0
.Li960R6:	ret
	.align 4
	.globl __Z12extractValueIhiLi31ELi19EET_T0_
	#  Function '_Z12extractValueIhiLi31ELi19EET_T0_'
	#  Registers used: g0 fp 
__Z12extractValueIhiLi31ELi19EET_T0_:
	shri	19,g0,g0
	and	31,g0,g0
.Li960R7:	ret
