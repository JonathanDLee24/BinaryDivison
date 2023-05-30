#Author Jonathan Lee
#Professor Devin Cook
#CSC35
#4-22-23
#BinaryDivison.asm
#This program will perform division of signed numbers it's alot to take in but it has the following rules 
#1. No use of idvi or imul div or mul stack or FPU MMX or any of the floating point instructions or stack registers
#2. Do not just use sub over and over
#3. must work like long division
#4. must work from left to right when doing long division
#5. you can use shift/roll 
#6. try to think like an intel staff when they created instructions on a blank CPU
#7. try to minimize use of registers if possible
#8. must have zerors work for 1/90 as an example 1/100
#9. must work with both negative and positive numbers
#10. the goal is to learn as much as possible with this.

.intel_syntax noprefix

.data

Get_Dividend:
	.asciz "Enter your Dividend: "

Get_Divisor:
	.asciz "Enter your Divisor: "

Continue:
	.asciz "Continue (y/n): "

Remain:
	.asciz "Remainder: "

NewLine:
	.asciz "\n"

Div:
	.asciz " / "

Equals:
	.asciz " = "

Dec:
	.asciz "."

Negative_Symbol:
	.asciz "-"

ZeroCondition:
	.asciz "Can't divide by 0 \n"

Get_Round:
	.asciz "How many positions would you like to round to? "

Warning:
	.ascii "********************************************* Manual Floats With Assembly *********************************************\n\n\n"
	.ascii "\t\tThis program can perform whole number division and return a decimal based result both postive and negative numbers.\n\n"
	.ascii "\t\tThe Goal of this program was to help build a solid understanding of assembly code and use what we learned in class.\n\n" 	
	.asciz "\t\tThis program does not use any intel mul, div, idiv, or imul mnemonics. It also does not use stacks, or MMX FPU \n\n\n"
.text          					

.global _start

_start:
	call ClearScreen
	mov rax, 0
	mov rbx, 0
	mov rcx, 0
	mov rdx, 0
	mov rsi, 0
	mov rdi, 0
	mov rbp, 0
	mov r8, 0
	mov r9, 0
	mov r10, 0
	mov r11, 0
	mov r12, 0
	mov r13, 0
	mov r14, 0
	mov r15, 0
	lea rdi, Warning
	call WriteString
#-----------------------------------------> Get Inputs <--------------------------------------------
#Get input from user	
Dividend:
	lea rdi, Get_Dividend
	call WriteString
	call ReadInt
	mov rax, rdi
	jmp Divisor

Zero:
	lea rdi, ZeroCondition
	call WriteString
#Get input from user
Divisor:
	lea rdi, Get_Divisor
	call WriteString
	call ReadInt	
	mov rbx, rdi
	cmp rbx, 0
	je Zero

#Output first part of equation
Output:
	mov cl, 63
	mov rdi, rax
	call WriteInt
	lea rdi, Div
	call WriteString
	mov rdi, rbx
	call WriteInt
	lea rdi, Equals
	call WriteString

#checks for negative numbers and sets flag r10
Neg_Rax:
	cmp rax, 0
	jge Neg_Rbx
	neg rax
	inc r10

Neg_Rbx:
        cmp rbx, 0
        jge loop_start
        neg rbx
        inc r10
#------------------------------------> Division Area whole number <--------------------------------------------
#Start of run loop_start will check rbx with r11 and shift rax until its greater or equal and jump to sub
loop_start:
	mov r11, rax
	shr r11, cl 
	cmp r11, rbx
	jge Sub
	cmp cl, 0
	je Remainder
	dec cl
	jmp loop_start

#Once r11 is greater or equal to rbx we can subtract it from rax and add one bit to r12 and shift r12 with the zeros via cl
Sub:
	mov r8, rbx
	shl r8, cl #extends rbx with zeros from cl	
	sub rax, r8 
	mov r12, 1 # put 1 into empty r12
	shl r12, cl #extends r12 with zerors from cls
	add r15, r12 #add into r15
	cmp rax, 0
	je End_Loop
	jmp loop_start

#Once cl reaches 0 will will display the remainder and exit
Remainder:
        cmp r10, 1 # r10 is used as a flag for negatives 0, 1, or 2 0 is postive 1 always negative and 2 always positive
        je Neg_Result
        jmp Rem

Neg_Result:
        neg r15 #make r15 or result register negative
	#neg rax#for use with float disabled shows remainder as negative
#start of decimal remainder output
Rem:
	mov rdi, r15
	call WriteInt
	lea rdi, NewLine
	call WriteString
	lea rdi, Remain
	call WriteString
	mov rdi, rax
	call WriteInt
	lea rdi, NewLine
	call WriteString
	call WriteString
	#call Exit #if enabled program will act like idiv and show int remainder and exit. for use if you do not run decimal remainder

#-------------------------------------------Area for floating division------------------------------------------
#start of decimal remainder (-/+)
	cmp r15, 0 	#if r15 is zero we cant negate it if we have a negative zero this cmp is for that conditon
        je If		#if r15 is zero we have to check r10 and add a negative symbol manually
	jmp Else	#if r15 is not zero just continue on as it can be negated or reflect a negative number already
If:
        cmp r10, 0	#r10 used as flag for negative inputs conditon for no negatives
        je Else
	cmp r10, 2	#r10 both as flag for both negative inputs
	je Else
        lea rdi, Negative_Symbol
        call WriteString

#start of output R15.RAX
Else:
	mov rdi, r15 #write Int value
	call WriteInt
	lea rdi, Dec #write decimal point
	call WriteString
	mov cl, 0 #set for use with scale
	mov r15, 0 #use with end value we will print 
	mov r9, 0

#now get float .RAX??????
	jmp Mul #lets scale up the remainder

#use with no remainder values
End_Loop:
	cmp r10, 1
	je Negate
	jmp End	

#negative number use non zero r15 values
Negate:
	neg r15 #make result register r15 negative based on r10 flag set to 1
	jmp End

#--------------------------------------->End area results output<------------------------------------
#ends program or restarts
End: 	
	call Zeros	#call zeros subroutine put missing zeros back into rax this is because all zeros left of last left value were chopped off  
	mov rdi, r15	#print the decimal remainder after zeros are added or tested if result needs them
	call WriteInt	#result value output completed 
	lea rdi, NewLine
	call WriteString
	call WriteString

#get user answer run again?
Run_Again:
	lea rdi, Continue
        call WriteString
        call ReadChar
        cmp dil, 'n'
	je Quit
        cmp dil, 'y' 
	je _start
	jmp Run_Again	#if n or y not entered runs loop

#exits program
Quit:
	call Exit

#------------------------->loop area for decimal remainder<----------------------------------------------------
#runs after mul scales
rem_loop_start:
	mov r11, r14	#scaled remainder now is checked for each bit to see if rbx can be subtracted
	shr r11, cl	#start high for cl
        cmp r11, rbx	#check can it subtract
        jge rem_Sub 
	cmp cl, 0 	#is it at end of cl
	jle End 	#if so scale again
	dec cl 		#decrement cl one
	jmp rem_loop_start #reloop this until exit

#Once r11 is greater or equal to rbx we can subtract it from rax and add one bit to r12 and shift r12 with the zeros via cl
rem_Sub: 
   	mov r8, rbx
        shl r8, cl 	#extends rbx with zeros from cl
        sub r14, r8 	#remove value
        mov r12, 1 	#put 1 into empty r12
        shl r12, cl 	#extends r12 with zerors from cls
        add r15, r12 	#add into r15
	cmp r14, 0
        je End
	dec cl
        jmp rem_loop_start

#------------------------------>Multiplier Section with adding only<--------------------------------

#convert binary to dec for floats division does not use mul or imul all binary
Mul:
    	mov r10, rax 		#mov remainder into r10 for use with multiplier
        mov r11, 10000000000000 #mov scale into r11 to 14 digits we want it scaled up in decimal if you change this you must also change repeater value also that is currently set to 13 digits
        mov cl, 63

main_loop:
	mov rbp, 1      # Flag remainder on if this is not enabled unit will skip Zeros check for whole numbers
	mov rax, 0	# clears rax each run just in case
        mov al, r11b    # moves 8 bits into al from number 1
        shl al, 7	# moves lsb to msb and results in all zeros after
        shr al, 7	# place msb bit back to lsb only one bit remains
        shr r11, 1	# shift r11 right once to chop off the bit that we just used
        cmp al, 1	# we are checking al to see if it's equal to 1 for adding
        je one          # if it's equal to one we will add it into the multiply r15 reg
        cmp r11, 0	# exit condtion once r11 reaches zero
        jle rem_loop_start  # jumps to end for exit condition
        shl r10, 1	# fence post condition shift r10 left incase we did not add
        jmp main_loop   # jump to loop
one:
    	add r14, r10    # adds our value if bit is one for lsb
        shl r10, 1	# shift left r10 1 bit
        jmp main_loop   # jump to main


#-------------------------------->Find Floating point subroutine<-------------------------------------

Zeros: #check size of remainder for digit size
	cmp rbp, 0
	je Zero_End

	mov rcx, 0
	mov r13, 0
	mov rdx, 0
	mov rdx, r15 	#mov r15 remainder into rdx to check how many digits it is
	mov r9, 0
	mov r10, 0

#checks how many digits are in value and if it is not equal to correct size adds zeros on scales it back
SetUp_Loop:
	mov cl, 63
	mov r13, rdx
	mov rbx, 10
	inc r10
	mov rdx, 0

#finds digits in r15 remainder from orginal run	
Zero_loop_start:
	mov r11, r13
	shr r11, cl
        cmp r11, rbx
        jge Sub_Zero
        cmp cl, 0
	jle Out_Put_Zeros
        dec cl
	jmp Zero_loop_start

#Once r11 is greater or equal to rbx we must set up rdx with value 
Sub_Zero:
        mov r8, rbx
        shl r8, cl #extends rbx with zeros from cl
	sub r13, r8
	mov r9, 1
	shl r9, cl
	add rdx, r9
	cmp r13, 0
	jle Out_Put_Zeros
       	jmp Zero_loop_start

#Now we have the first digit removed we must redo the loop with rdx one digit less counting the digits each loop
Out_Put_Zeros:
	cmp rdx, 0
	jg SetUp_Loop

	mov rdi, 0
repeater:
	cmp r10, 13
	jge Zero_End
	call WriteInt
	inc r10
	jg repeater

Zero_End:
	ret

