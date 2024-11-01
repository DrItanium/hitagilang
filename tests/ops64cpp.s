	.text
	.align 4
	.globl __ZanRK1BS1_
	#  Function '_ZanRK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 g13 fp r4* 
	#		   
__ZanRK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	ld	12(g0),g2
	ld	12(g1),r4
	ld	(g0),g6
	ld	(g1),g13
	ld	4(g0),g5
	ld	4(g1),g3
	ld	8(g0),g4
	ld	8(g1),g7
	and	g5,g3,g5
	and	g2,r4,g2
	and	g6,g13,g6
	and	g4,g7,g4
	st	g6,64(fp)
	st	g5,68(fp)
	st	g4,72(fp)
	st	g2,76(fp)
	ldq	64(fp),g0
.Li960R1:	ret
	.align 4
	.globl __ZorRK1BS1_
	#  Function '_ZorRK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 g13 fp r4* 
	#		   
__ZorRK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	ld	12(g0),g2
	ld	12(g1),r4
	ld	(g0),g6
	ld	(g1),g13
	ld	4(g0),g5
	ld	4(g1),g3
	ld	8(g0),g4
	ld	8(g1),g7
	or	g5,g3,g5
	or	g2,r4,g2
	or	g6,g13,g6
	or	g4,g7,g4
	st	g6,64(fp)
	st	g5,68(fp)
	st	g4,72(fp)
	st	g2,76(fp)
	ldq	64(fp),g0
.Li960R2:	ret
	.align 4
	.globl __ZeoRK1BS1_
	#  Function '_ZeoRK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 g13 fp r4* 
	#		   
__ZeoRK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	ld	12(g0),g2
	ld	12(g1),r4
	ld	(g0),g6
	ld	(g1),g13
	ld	4(g0),g5
	ld	4(g1),g3
	ld	8(g0),g4
	ld	8(g1),g7
	xor	g5,g3,g5
	xor	g2,r4,g2
	xor	g6,g13,g6
	xor	g4,g7,g4
	st	g6,64(fp)
	st	g5,68(fp)
	st	g4,72(fp)
	st	g2,76(fp)
	ldq	64(fp),g0
.Li960R3:	ret
	.align 4
	.globl __ZcoRK1B
	#  Function '_ZcoRK1B'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp 
__ZcoRK1B:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	ld	12(g0),g7
	ldt	(g0),g4
	not	g4,g4
	not	g5,g5
	not	g6,g6
	not	g7,g7
	stq	g4,64(fp)
	ldq	64(fp),g0
.Li960R4:	ret
	.align 4
	.globl __Z5nand0RK1BS1_
	#  Function '_Z5nand0RK1BS1_'
	#  Registers used: g0 g1 g2 g3 fp sp 
	#		   r3* 
__Z5nand0RK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	callx	__ZanRK1BS1_
	stq	g0,64(fp)
	lda	64(fp),g0
	callx	__ZcoRK1B
.Li960R5:	ret
	.align 4
	.globl __Z5nand1RK1BS1_
	#  Function '_Z5nand1RK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 g13 fp r4* 
	#		   
__Z5nand1RK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	ld	12(g0),g2
	ld	12(g1),r4
	ld	(g0),g6
	ld	(g1),g13
	ld	4(g0),g5
	ld	4(g1),g3
	ld	8(g0),g4
	ld	8(g1),g7
	nand	g5,g3,g5
	nand	g2,r4,g2
	nand	g6,g13,g6
	nand	g4,g7,g4
	st	g6,64(fp)
	st	g5,68(fp)
	st	g4,72(fp)
	st	g2,76(fp)
	ldq	64(fp),g0
.Li960R6:	ret
