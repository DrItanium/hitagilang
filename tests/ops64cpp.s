	.text
	.align 4
	.globl __ZanRK1BS1_
	#  Function '_ZanRK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp 
__ZanRK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	lda	64(fp),g3
	ld	(g0),g2
	ld	(g1),g4
	and	g2,g4,g2
	ld	4(g0),g7
	ld	4(g1),g4
	and	g7,g4,g7
	ld	8(g0),g6
	ld	8(g1),g4
	and	g6,g4,g6
	ld	12(g0),g5
	ld	12(g1),g4
	and	g5,g4,g5
	st	g2,64(fp)
	st	g7,4(g3)
	st	g6,8(g3)
	st	g5,12(g3)
	ldq	64(fp),g0
.Li960R1:	ret
	.align 4
	.globl __ZorRK1BS1_
	#  Function '_ZorRK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp 
__ZorRK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	lda	64(fp),g3
	ld	(g0),g2
	ld	(g1),g4
	or	g2,g4,g2
	ld	4(g0),g7
	ld	4(g1),g4
	or	g7,g4,g7
	ld	8(g0),g6
	ld	8(g1),g4
	or	g6,g4,g6
	ld	12(g0),g5
	ld	12(g1),g4
	or	g5,g4,g5
	st	g2,64(fp)
	st	g7,4(g3)
	st	g6,8(g3)
	st	g5,12(g3)
	ldq	64(fp),g0
.Li960R2:	ret
	.align 4
	.globl __ZeoRK1BS1_
	#  Function '_ZeoRK1BS1_'
	#  Registers used: g0 g1 g2 g3 g4 g5 g6 g7 fp 
__ZeoRK1BS1_:
	addo	16,sp,sp
	#Prologue stats:
	#  Total Frame Size: 16 bytes
	#  Local Variable Size: 16 bytes
	#End Prologue#
	lda	64(fp),g3
	ld	(g0),g2
	ld	(g1),g4
	xor	g2,g4,g2
	ld	4(g0),g7
	ld	4(g1),g4
	xor	g7,g4,g7
	ld	8(g0),g6
	ld	8(g1),g4
	xor	g6,g4,g6
	ld	12(g0),g5
	ld	12(g1),g4
	xor	g5,g4,g5
	st	g2,64(fp)
	st	g7,4(g3)
	st	g6,8(g3)
	st	g5,12(g3)
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
	lda	64(fp),g1
	ld	(g0),g7
	not	g7,g7
	ld	4(g0),g6
	not	g6,g6
	ld	8(g0),g5
	not	g5,g5
	ld	12(g0),g4
	not	g4,g4
	st	g7,64(fp)
	st	g6,4(g1)
	st	g5,8(g1)
	st	g4,12(g1)
	ldq	64(fp),g0
.Li960R4:	ret
