.model tiny

.code

org 100h

locals

VIDEOSEG equ 0b800h
X_START	 equ 40d
Y_START	 equ 5d


PROFILE_MODE equ 0d
ONE_FRAME_MODE equ 1d
TWO_FRAME_MODE equ 2d

HIGH_FR equ 3d
LENGTH_FRAME equ 9d


HOT_KEY_F1 equ 3Bh
HOT_KEY_F2 equ 3Ch

SAVE_SEG equ 00AAh
FRAME_FLAG_PTR equ 0
SAVED_IMAGE_PTR equ 2

Start:
            cli
            
            xor bx, bx
            mov es, bx


            mov bx, 09h * 4
            mov ax, es:[bx]                         ;saved orig09
            mov word ptr Orig09, ax
            mov ax, es:[bx + 2]
            mov word ptr Orig09 + 2, ax
            
            mov word ptr es:[bx], offset New09      ;send new 09 in memory
            push cs
            pop ax
            mov word ptr es:[bx + 2], ax


            mov bx, 08h * 4
            mov ax, es:[bx]                         ;saved orig08
            mov word ptr Orig08, ax
            mov ax, es:[bx + 2]
            mov word ptr Orig08 + 2, ax
            
            mov word ptr es:[bx], offset New08      ;send new 08 in memory
            push cs
            pop ax
            mov word ptr es:[bx + 2], ax

            sti

            mov ax, 3100h
            mov dx, offset EndProg                      ;save program in memory
            shr dx, 4
            inc dx
            int 21h


;---------------------------------------------------------- 
; New 09 interrupt
;  
; Entry: DI - ptr of string
;        ES - segment
;        BX - length
; Note:  
; Exit:  NONE
; Destr: CX, AX, DI, BX
;---------------------------------------------------------- 

New09       proc

            push ax di es

            in al, 60h
            cmp al, HOT_KEY_F1
            je @@MakeMyCode
            cmp al, HOT_KEY_F2
            je @@MakeMyCode

            pop es di ax

            db 0EAh
Orig09      dd 0

@@MakeMyCode:   
            push ax

            in   al, 61h
            mov  ah, al
            or   al, 80h
            out  61h, al
            xchg ah, al
            out  61h, al

            mov al, 20h
            out 20h, al

            pop ax
            
            xor di, di
            cmp al, HOT_KEY_F1
            je @@skip_new_flag
            mov di, 1

@@skip_new_flag:

            mov ax, SAVE_SEG
            mov es, ax
            mov ax, di
            mov di, FRAME_FLAG_PTR
            mov es:[di], al

            pop es di ax
            iret

            endp


;---------------------------------------------------------- 
; New 08 interrupt
;  
; Entry: DI - ptr of string
;        ES - segment
;        BX - length
; Note:  
; Exit:  NONE
; Destr: CX, AX, DI, BX
;---------------------------------------------------------- 
New08       proc

            push di es cx
            push ax

            mov ax, SAVE_SEG
            mov es, ax
            mov di, FRAME_FLAG_PTR
            mov al, 1
            cmp es:[di], al
            je @@MakeMyCode

            call SaveImage
            call ReturnImage

            pop ax
            pop cx es di

            db 0EAh
Orig08      dd 0
            

@@MakeMyCode:
            call SaveImage
            call ReturnImage
            
            pop ax
            push ax
            call Draw

            mov cx, 0FFFh
@@delay:
            nop
            nop
            nop
            loop @@delay

            mov al, 20h
            out 20h, al

            pop ax
            pop cx es di
            iret
            
            endp

;---------------------------------------------------------- 
; SaveImage
;  
; Entry: None
; Note:  ES = videoseg addr (0b800h) 
; Exit:  None 
; Destr: AX, BX, ES, SI
;---------------------------------------------------------- 

SaveImage  proc

            push ax bx cx es di dx ds si

            mov ax, SAVE_SEG
            mov es, ax
            mov di, SAVED_IMAGE_PTR
            mov dx, (Y_START * 80d + X_START) * 2
            push dx
            pop si
            mov dx, VIDEOSEG
            push dx
            pop ds
            mov bx, 5
            mov cx, LENGTH_FRAME
            add cx, cx
            

@@cpy1:
            repnz movsb
            mov cx, LENGTH_FRAME
            add cx, cx
            add di, 160d
            sub di, cx
            add si, 160d
            sub si, cx
            sub bx, 1
            cmp bx, 0
            jne @@cpy1

            pop si ds dx di es cx bx ax
            ret
            endp

ReturnImage proc
            push ax bx cx es di dx ds si

            mov ax, VIDEOSEG
            mov es, ax
            mov di, (Y_START * 80d + X_START) * 2
            mov dx, SAVED_IMAGE_PTR
            push dx
            pop si
            mov dx, SAVE_SEG
            push dx
            pop ds
            mov bx, 5
            mov cx, LENGTH_FRAME
            add cx, cx

@@cpy2:
            repnz movsb
            mov cx, LENGTH_FRAME
            add cx, cx
            add di, 160d
            sub di, cx
            add si, 160d
            sub si, cx
            sub bx, 1
            cmp bx, 0
            jne @@cpy2

            pop si ds dx di es cx bx ax
            ret
            endp

;---------------------------------------------------------- 
; Draw
;  
; Entry: None
; Note:  ES = videoseg addr (0b800h) 
; Exit:  None 
; Destr: AX, BX, ES, SI
;---------------------------------------------------------- 

Draw        proc

            push di dx si es
            
            push ds
            push cs
            pop ds

            call Draw_Frame

            pop ds

            pop es si dx di 

            ret
            endp

;---------------------------------------------------------- 
; Draw a frame 
;  
; Entry: AH - color (attr) of line 
;        CX - lenght of string 
;        SI - addr of 3-byte array containing line elements 
;        DI - adress of start of line 
; Note:  ES = videoseg addr (0b800h) 
; Exit:  None 
; Destr: AL, CX, SI, BX, DX
;---------------------------------------------------------- 
Draw_Frame		proc
                push cx
                push bx
                push ax

                push cx
                push bx
                push ax


				mov ax, VIDEOSEG
				mov es, ax

                mov al, 2d
				mov ah, 3d

				mov si, offset Two_Frame_Symbols
				mov di, (Y_START * 80d + X_START) * 2
				mov cx, LENGTH_FRAME
				mov bx, cx
				call DrawLine
				mov cx, bx
				mov dx, HIGH_FR
Draw_middle:
				call DrawLine
				mov cx, bx
				sub si, 3
				sub dx, 1
				cmp dx, 0
				jne Draw_middle

				add si, 3
				call DrawLine
;------

                
                mov si, offset buffer
                sub si, 3*3
                mov di, ((Y_START + 1) * 80d + X_START + 1) * 2
                mov cx, 3
                mov ah, 3d

@@Draw_letter_regs:
				lodsb
                stosw
				lodsb
                stosw
				lodsb
                stosw
                add di, 160d - 6d
                loop @@Draw_letter_regs

                mov bl, 0 ; nums of regs on table
@@rep_new_reg:
                mov di, offset buffer
                pop ax 
                mov dx, ax
                mov cl, 4

                push es
                push cs
                pop es
                call Itoa
                pop es
                

                mov si, offset buffer
                mov di, ((Y_START + 1) * 80d + X_START + 4) * 2
                mov ax, bx
                mov bl, 160d
                mul bl
                add di, ax
                div bl
                mov bl, al
                mov cx, 4d
                mov ah, 3d

@@Draw_letter_nums:
				lodsb
                stosw
                loop @@Draw_letter_nums
                
                add di, 160d - 4d
                add bl, 1
                cmp bl, 3
                jne @@rep_new_reg

                pop ax
                pop bx
                pop cx

				ret 
				endp

;---------------------------------------------------------- 
; Draw a line in a frame 
;  
; Entry: AH - color (attr) of line 
;        CX - lenght of string 
;        SI - addr of 3-byte array containing line elements 
;        DI - adress of start of line 
; Note:  ES = videoseg addr (0b800h) 
; Exit:  None 
; Destr: AL, CX, SI
;---------------------------------------------------------- 
DrawLine        proc             
 
                mov al, [si]
                inc si 
                mov es:[di], ax
                add di, 2 
 
                mov al, [si]
                inc si 
 
                sub cx, 2 
                jbe @@ret
 
@@nextSym:      mov es:[di], ax
                add di, 2 
                loop @@nextSym

                mov al, [si]
				inc si
                mov es:[di], ax
                add di, 2 

				add di, 160d
				sub di, bx
				sub di, bx

@@ret:          ret
				endp


Two_Frame_Symbols:			db 	0C9h, 0CDh, 0BBh, 0BAh, 020h, 0BAh, 0C8h, 0CDh, 0BCh

One_Frame_Symbols:			db 	0DAh, 0C4h, 0BFh, 0B3h, 020h, 0B3h, 0C0h, 0C4h, 0D9h

                            db 'AX '
                            db 'BX '
                            db 'CX '
buffer:                     db '0000'

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------- 
; ItoaInclude
;  
; Entry: DI - ptr of string
;        ES - segment
;        BL - type of num
;        DX - NUM
;        Cl -shift type
; Note:  
; Exit:  AX - ptr of string
; Destr: CX, BX, DX
;---------------------------------------------------------- 


Itoa                proc

                    push dx
                    push bx
                    push cx

                    call ItoaInclude2xD

                    pop cx
                    pop bx
                    pop dx

                    ret
                    endp

ItoaInclude2xD      proc
                    
                    mov ax, dx
                    push di

                    add di, 3

                    xor ch, ch
                    cmp cl, 4
                    je @@SetCh4
                    cmp cl, 3
                    je @@SetCh3
                    jmp @@SetCh1

@@SetCh4:           
                   mov ch, 1111b 
                   jmp @@while

@@SetCh3:
                   mov ch, 111b 
                   jmp @@while
@@SetCh1:
                   mov ch, 1b 

@@while:             
                    mov dx, ax ; save result = ax
                    and dl, ch ;bl ; найти остаток

                    shr ax, cl

                    add dl, '0'
                    cmp dl, '9'
                    jbe @@SkipLet
                    sub dl, '0'
                    add dl, 'A'
                    sub dl, 10d

@@SkipLet:
                    mov byte ptr es:[di], dl
                    sub di, 1

                    cmp al, 0
                    ja @@while

                    pop cx
                    mov bx, di
                    sub bx, cx
                    mov di, cx

@@ret:              
                    mov di, cx
                    mov ax, di
                    ret 
                    endp

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------

EndProg:
end Start


