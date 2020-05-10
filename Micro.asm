        .MODEL huge
        .STACK 64
        .DATA
VALUE DB ?
SPLIT DB '--------------------------------------------------------------------------------$'
SPLIT2 DB '----------------------------------------$'
POSUPX DB 0
POSDOWNX DB 0
POSUPY DB 0
POSDOWNY DB 11
inname db "Enter your Name :$"
jerrylifes db 1
jerrycheese db 0
gamecheese db 4
tomtraps db 0
AsciiSendValue db ?
AsciiReadValue db ?
gamemode db 2
LEVEL DB 2
CharacterSend DB (?)
CharacterReceived DB 0FFh
chatrequest DB 0
gamerequest DB 0
WHICHPLAYER DB 'To play with jerry press "J" || To play with tom press "T"$'
SENTCHATINV DB 'you sent a chat invitation to$'
WHICHlevel  DB 'To choose level 1 press "1" || To choose level 2 press "2"$'
SENTGAMEINV DB 'you sent a GAME invitation to$'
RECEIVECHATINV DB 'invite you to chat, press 2$'
RECEIVEGAMEINV DB 'invite you to play, press 2$'

LeaveGame DB 0 ; 0 leave the game 1 in game
playerType db 0 ;;0 sent invitation, 1 received invitation

INVITE DB 2;1 YOU SENT AN INVITE FOR GAME,0 YOU HAVE AN INVITE
;INVITECHAT DB 2;1 YOU SENT AN INVITE FOR GAME,0 YOU HAVE AN INVITE

Accepted db 0
acceptchat db 0

Myname DB 10 dup(' '),'$'
othername DB 10 dup(' '),'$'
jerryfreeze db 0
player dW 0 ;0 if jerry 1 if tom 2 if thunder
TOM DB "TOM    $"
JERRY DB "JERRY  $"
THMODE DB "THUNDER$"
countgrid dw 0
xx dw 43
yy dw 43
	prevxup db 0
	prevyup db 0
	prevxdown db 0
	prevydown db 13
	endrequest db 0
Readtest db 0
EndEscape db 0
xnew dw 43
ynew dw 43
GameEnd db 0
cell dw ?
x dw ?
y dw ?	
sendvalue db ?
readvalue db ?
Endseconds db 0
xtom dw 275
ytom dw 159
xtomn dw 275
Greycolor db 7h
ytomn dw 159
tomtest db 1
celltom dw 44
cleartom db 0h
tomcolor db 9h
celljerry dw 0
insidegrid db 0
celltomold dw 44
celljerryold dw 0
clearcolor db 0fh
browncolor db 6h	
cheeseclr db 0eh
xcheese dw 101
ycheese dw 43
testjerry db 1
gridarr db 45 dup(0)
randomarr db 3,2,5,6,7,9,10,14,17,21,22,25,36,37,24,39,35
weapons db   3,5,1,5,1,0,0,1,0,0,5,7,0,1,5,1,4
weaponslevel2 db   3,5,1,5,1,0,2,1,0,0,5,7,2,1,5,1,4

ORIGINAL db 45 dup(0)
losejerry db 0
winjerry db 0
randnum db 5
Tomtrapstr db "TOM Traps :$"
CO_X DW 30
CO_Y DW 30
LEN DW 26,20
LENEND DW 0
LENSTART DW 0
CHOSENCOLOR DB 0
CURRENTWIDTH DB 0
GRIDCOLOR DB  45 dup(0fh)
GridWhiteColor db 0fh
snowcolor  db 0BH
wintom db 0    
GAMENAME DB "TOM&JERRY$"
PLAYNOW DB "PLAYNOW   PREES:2$"
HOWTOPLAY DB "HOWTOPLAY PRESS:1$"
ENDGAME DB "ENDGAME$"
CHATMODE DB "CHATMODE  PREES:3$"
EXITGAME DB "EXITGAME  PREES:4$"
done DB "THE GAME IS OVER"
jerrywin DB "JERRY WINS$"
tomwin DB "TOM WINS$"
injerry db "Enter Jerry Player Name :$"
intom db "Enter Tom Player Name :$"
jerryplayer DB 10 dup(' '),'$'
tomplayer DB 10 dup(' '),'$'
SPACE DB 10 dup(' '),'$'
COUNT DW 9
HOLD DW 0
COLON DB  ":"
LIVE DB "LIVES$" 
currentsecond db ?  
ThunderFlag db 0 
Freezecounter db 0
CheeseWidth equ 24
CheeseHeight equ 19
WidthIMG equ 24
Height equ 24 
heartWidth equ 24
heartHeight equ 21
logoWidth equ 114
logoHeight equ 64
wi dw 0
he dw 0
BEGX DW ?
logoFilename DB 'logo.bin', 0
logoFilehandle DW ?
logoData DB logoWidth*logoHeight dup(0)
cheeseFilename DB 'cheese.bin', 0
cheeseFilehandle DW ?
cheeseData DB cheeseWidth*cheeseHeight dup(0)

jerryFilename DB 'jerry.bin', 0
jerryFilehandle DW ?
jerryData DB WidthIMG*Height dup(0)


thunderFilename DB 'thunder.bin', 0
thunderFilehandle DW ?
thunderData DB WidthIMG*Height dup(0)

heartFilename DB 'heart.bin', 0
heartFilehandle DW ?
heartData DB heartWidth*heartHeight dup(0)

tomFilename DB 'tom.bin', 0
tomFilehandle DW ?
tomData DB WidthIMG*Height dup(0)
playertomstr db "TOM$"
playerjerrystr db "JERRY$"
HOWTOPLAY1 DB 'FOR JERRY:1)try to eat all cheese to win$'
HOWTOPLAY2 DB '          2)try to escape from tom$'
HOWTOPLAY3 DB '          3)you have 2 weapons:$' 
HOWTOPLAY4 db '            "freeze" & "remove trap"$'
HOWTOPLAY5 DB 'FOR TOM:  1)try to eat jerry to win$'
HOWTOPLAY6 DB '          2)you have "Trap" weapon to$'
HOWTOPLAY7 DB '            trap jerry$'
HOWTOPLAY8 DB ' press escape to return to the menu$'

.Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Cheesemusic proc far
push ax
push bx
push cx
push dx
sound :     

MOV     DX,2000          ; Number of times to repeat whole routine.

MOV     BX,1             ; Frequency value.

MOV     AL, 10110110B    ; The Magic Number (use this binary number only)
OUT     43H, AL          ; Send it to the initializing port 43H Timer 2.

NEXT_FREQUENCY:          ; This is were we will jump back to 2000 times.

MOV     AX, BX           ; Move our Frequency value into AX.

OUT     42H, AL          ; Send LSB to port 42H.
MOV     AL, AH           ; Move MSB into AL  
OUT     42H, AL          ; Send MSB to port 42H.

IN      AL, 61H          ; Get current value of port 61H.
OR      AL, 00000011B    ; OR AL to this value, forcing first two bits high.
OUT     61H, AL          ; Copy it to port 61H of the PPI Chip
                         ; to turn ON the speaker.

MOV     CX, 100          ; Repeat loop 100 times
DELAY_LOOP:              ; Here is where we loop back too.
LOOP    DELAY_LOOP       ; Jump repeatedly to DELAY_LOOP until CX = 0


INC     BX               ; Incrementing the value of BX lowers 
                         ; the frequency each time we repeat the
                         ; whole routine

DEC     DX               ; Decrement repeat routine count

CMP     DX, 0            ; Is DX (repeat count) = to 0
JNZ     NEXT_FREQUENCY   ; If not jump to NEXT_FREQUENCY
                         ; and do whole routine again.

                         ; Else DX = 0 time to turn speaker OFF

IN      AL,61H           ; Get current value of port 61H.
AND     AL,11111100B     ; AND AL to this value, forcing first two bits low.
OUT     61H,AL           ; Copy it to port 61H of the PPI Chip
                         ; to
pop dx
pop cx
pop bx
pop ax
ret
cheesemusic endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CALCULATES THE VALUE OF THE XY COORDINATES GIVEN THE NO OF THE CELL IN THE GRID
calcxy proc 
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
mov bl,9  ;width of the grid(number of cells in a row)
mov bh,29  ; length between two cells 
inc cell 
mov ax,cell  ;cell and x and y must be words
dec ax
div bl

mul bh

add ax,43
mov y,ax

mov ax,cell
dec ax
div bl
mov al,ah
mul bh
add ax,43
mov x,ax
mov ax,x
mov bx,y
    POP DX
    POP CX
    POP BX
    POP AX
RET   
calcxy ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DRAWING THE LOGO PIC IN THE MENU
DRAWLOGO PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, LOGOFilename
    INT 21h
    MOV [LOGOFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [LOGOFilehandle]
    MOV CX,LOGOWidth*LOGOHeight ; number of bytes to read
    LEA DX, LOGOData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , LOGOData ; BL contains index at the current drawn pixel
    POP DX
    POP CX
    MOV he,dx
    MOV wi,cx
    add he,32
    add wi,57
    SUB CX,57       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,32
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopL:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopL 	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopL

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [LOGOFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWLOGO ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWCHEESE PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, cheeseFilename
    INT 21h
    MOV [cheeseFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [cheeseFilehandle]
    MOV CX,cheeseWidth*cheeseHeight ; number of bytes to read
    LEA DX, cheeseData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , cheeseData ; BL contains index at the current drawn pixel
    POP DX
    POP CX
    MOV he,dx
    MOV wi,cx
    add he,9
    add wi,12
    SUB CX,12       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,10
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopch:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopch 	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopch

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [cheeseFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWCHEESE ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWJERRY PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, JerryFilename
    INT 21h
    MOV [JerryFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [JerryFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, JerryData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , JerryData ; BL contains index at the current drawn pixel
    MOV DX,ynew
    MOV  CX ,xnew
    MOV he,dx
    MOV wi,cx
    add he,12
    add wi,12
    SUB CX,12       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,12
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopj:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopj 	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopj

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [jerryFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWJERRY ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearbuffer proc far
clearbf:
mov ah,1
int 16h
jz endclear
mov ah,0
int 16h
cmp ax,0
jmp clearbf
endclear:
clearbuffer endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWTOM PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, TOMFilename
    INT 21h
    MOV [TOMFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [TOMFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, TOMData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , TOMData ; BL contains index at the current drawn pixel
    MOV DX,YTOMN
    MOV  CX,XTOMN
    MOV he,dx
    MOV wi,cx
    add he,12
    add wi,12
    SUB CX,12       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,12
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopT:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopT	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopT

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [TOMFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWTOM ENDP
DISPLAYCHAR PROC
  PUSH AX
  PUSH BX
  PUSH CX
        MOV AH,2
	MOV BH,0
        MOV DL,value
        INT 21H
  POP CX
  POP BX
  POP AX
	RET
DISPLAYCHAR ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWthunder PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, thunderFilename
    INT 21h
    MOV [thunderFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [thunderFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, thunderData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , thunderData ; BL contains index at the current drawn pixel
    POP DX
    POP CX
    MOV he,dx
    MOV wi,cx
    add he,12
    add wi,12
    SUB CX,12       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,12
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopth:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopth	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopth

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [thunderFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWthunder ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DRAWheart PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, heartFilename
    INT 21h
    MOV [heartFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [heartFilehandle]
    MOV CX,heartWidth*heartHeight ; number of bytes to read
    LEA DX, heartData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX ,heartData ; BL contains index at the current drawn pixel
    POP DX
    POP CX
    MOV he,dx
    MOV wi,cx
    add he,11
    add wi,12
    SUB CX,12       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,10
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLooph:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLooph 	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLooph

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [heartFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWheart ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Freezetom proc far 
push ax
mov al,jerryfreeze
cmp al,1
jl Endfreeze
add Freezecounter,2
dec jerryfreeze

Endfreeze:
pop ax
ret
Freezetom endp
jerryturn proc far

checksendturn:
 IsUp: 
      cmp ah,72 
      jnz IsDown
	  mov cx,ynew
	  sub cx,13
	  cmp cx,30
	  jle UPEND
	  mov cx,celljerry
	  sub cx,9
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je UPEND
	  mov bx,xnew
	  mov xx,bx
	  mov bx,ynew
	  mov yy,bx
	  mov bx,celljerry
	  mov celljerryold,bx
      sub ynew,29
	  sub celljerry,9
	    
     UPEND:  
	  mov cl,1
	  mov testjerry,cl
	 
      jmp Endjerryturn          
IsDown:
      cmp ah,80
      jnz IsRight
	  
	  mov cx,ynew
	  add cx,13
	  cmp cx,172
	  jge DOWNEND
	  mov cx,celljerry
	  add cx,9
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je DOWNEND
	  
	  mov bx,xnew
	  mov xx,bx
	  mov bx,ynew
	  mov yy,bx
	  mov bx,celljerry
	  mov celljerryold,bx
      add ynew,29
	  add celljerry,9   	    
  DOWNEND:    
  	  mov cl,1
	  mov testjerry,cl
   
      jmp Endjerryturn
IsRight:
    cmp ah,77
    jnz IsLeft
    mov cx,xnew
	add cx,13
	cmp cx,288
	jge RIGHTEND
	mov cx,celljerry
	add cx,1
	mov ax,cx
    mov BX,offset gridarr
    XLAT     ;translate and put the result in AL
	cmp al,1
	je RIGHTEND
	  
	mov bx,xnew
    mov xx,bx
	mov bx,ynew
	mov yy,bx
	mov bx,celljerry
	mov celljerryold,bx
    add xnew,29
	add celljerry,1
	    
	
   RIGHTEND:   
   	mov cl,1
	mov testjerry,cl
   
    jmp Endjerryturn

IsLeft:
        cmp ah,75
		jz contleft
		mov ah,thunderflag  ;CLEAR BUFFER AFTER TOM TURN IN THUNDER BEFORE GOING TO JERRY
		cmp ah,1
		jz Endjerryturn
		
		jmp Endjerryturn
    contleft:
      mov cx,xnew
	  sub cx,13
	  cmp cx,30
	  jle FEND
	  mov cx,celljerry
	  sub cx,1
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je FEND
	  mov bx,xnew
	  mov xx,bx
	  mov bx,ynew
	  mov yy,bx
	  mov bx,celljerry
	  mov celljerryold,bx
      sub xnew,29
	  dec celljerry
     
   FEND:   
   	  mov cl,1
	  mov testjerry,cl
    
	  
Endjerryturn:
ret
jerryturn endp	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
jerryturnrecieve proc far

checksendturnrecieve:
 IsUprecieve: 
      cmp al,72 
      jnz IsDownrecieve
	  mov cx,ynew
	  sub cx,13
	  cmp cx,30
	  jle UPENDrecieve
	  mov cx,celljerry
	  sub cx,9
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je UPENDrecieve
	  mov bx,xnew
	  mov xx,bx
	  mov bx,ynew
	  mov yy,bx
	  mov bx,celljerry
	  mov celljerryold,bx
      sub ynew,29
	  sub celljerry,9
	    
     UPENDrecieve:  
	  mov cl,1
	  mov testjerry,cl
	  mov ax,0
      int 16h
      jmp Endjerryturnrecieve         
IsDownrecieve:
      cmp al,80
      jnz IsRightrecieve
	  
	  mov cx,ynew
	  add cx,13
	  cmp cx,172
	  jge DOWNENDrecieve
	  mov cx,celljerry
	  add cx,9
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je DOWNENDrecieve
	  
	  mov bx,xnew
	  mov xx,bx
	  mov bx,ynew
	  mov yy,bx
	  mov bx,celljerry
	  mov celljerryold,bx
      add ynew,29
	  add celljerry,9   	    
  DOWNENDrecieve:    
  	  mov cl,1
	  mov testjerry,cl
      mov ax,0
      int 16h
      jmp Endjerryturnrecieve
IsRightrecieve:
    cmp al,77
    jnz IsFrecieve
    mov cx,xnew
	add cx,13
	cmp cx,288
	jge RIGHTENDrecieve
	mov cx,celljerry
	add cx,1
	mov ax,cx
    mov BX,offset gridarr
    XLAT     ;translate and put the result in AL
	cmp al,1
	je RIGHTENDrecieve
	  
	mov bx,xnew
    mov xx,bx
	mov bx,ynew
	mov yy,bx
	mov bx,celljerry
	mov celljerryold,bx
    add xnew,29
	add celljerry,1
	    
	
   RIGHTENDrecieve:   
   	mov cl,1
	mov testjerry,cl
    mov ax,0
    int 16h
    jmp Endjerryturnrecieve
isFrecieve:
       cmp aL,66H
       jnz ISCAPITALFrecieve
	   jmp FPRESSEDrecieve
	   ISCAPITALFrecieve:
	   cmp al,46h
	   jnz isLeftrecieve
	FPRESSEDrecieve:
	  call Freezetom
      
   	  
      mov ax,0
      int 16h
      jmp Endjerryturnrecieve
IsLeftrecieve:
        cmp al,75
		jz contleftrecieve
		mov ah,thunderflag  ;CLEAR BUFFER AFTER TOM TURN IN THUNDER BEFORE GOING TO JERRY
		cmp ah,1
		jz Endjerryturnrecieve
		mov ax,0
		int 16h
		jmp Endjerryturnrecieve
    contleftrecieve:
      mov cx,xnew
	  sub cx,13
	  cmp cx,30
	  jle FENDrecieve
	  mov cx,celljerry
	  sub cx,1
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je FENDrecieve
	  mov bx,xnew
	  mov xx,bx
	  mov bx,ynew
	  mov yy,bx
	  mov bx,celljerry
	  mov celljerryold,bx
      sub xnew,29
	  dec celljerry
     
   FENDrecieve:   
   	  mov cl,1
	  mov testjerry,cl
      mov ax,0
      int 16h
	  
Endjerryturnrecieve:
call drawjerry
ret
jerryturnrecieve endp	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SETCURSOR PROC
 PUSH AX
 push bx
 PUSH CX
 PUSH DX  
	MOV BH,0
        MOV AH,2
        MOV DH,23
        INT 10H
POP DX
POP CX
pop bx
POP AX
	RET
SETCURSOR ENDP 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DISPLAYSTRING MACRO OUTMSG
		push ax
		push dx
                MOV AH,09H
                MOV DX,OFFSET OUTMSG
                INT 21H
		pop dx 
		pop ax
ENDM DISPLAYSTRING 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearcell proc far
      mov cx,xnew
	  mov dx,ynew
	  sub cx,13
	  sub dx,13
	  MOV SI,OFFSET GridWhiteColor
	 
	ret
clearcell endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWCELL proc far
	
    MOV LENEND,DX
    MOV DI,LEN
    ADD LENEND,DI
    MOV LENSTART,CX  
   YAXIS:
       XAXIS:
        MOV AL,[SI] ;CHOSEN COLOUR
        MOV AH,0CH
        INT 10H
        INC CX
        DEC BX        	
       JNZ XAXIS
     MOV BX,LEN
     MOV CX,LENSTART
     INC DX
     CMP DX,LENEND
    JNZ YAXIS
    INC SI
    MOV CX,LENSTART
    ADD CX,DI
    MOV DX,LENEND
    ADD DX,DI
	ret
DRAWCELL ENDp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWTRAP proc far
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    MOV CO_Y,DX
    MOV  CO_X,CX 
    SUB CX,8
    ADD DX,7
    MOV DI,DX
    ADD DI,2
    MOV LENSTART,CX  
    MOV AL,6
   TRAPHEIGHT:
     MOV BX,16
       TRAPWIDTH:
        INT 10H
        INC CX
        DEC BX        	
       JNZ TRAPWIDTH
     MOV CX,LENSTART
     INC DX
     CMP DX,DI
    JNZ TRAPHEIGHT
    ;;;;;;;;;;;;;;;;;
    MOV  DX,CO_Y
    MOV  CX,CO_X 
    SUB CX,5
    ADD DX,4
    MOV DI,DX
    ADD DI,3
    MOV LENSTART,CX  
    MOV AL,0EH
   TRAPCHH:
     MOV BX,3
       TRAPCHW:
        INT 10H
        INC CX
        DEC BX        	
       JNZ TRAPCHW
     MOV CX,LENSTART
     INC DX
     CMP DX,DI
    JNZ TRAPCHH
;;;;;;;;;;;;;;;;;
    MOV  DX,CO_Y
    MOV  CX,CO_X 
    SUB CX,5
    SUB DX,4
    MOV BX,11
    MOV AL,8
    T1:              
       INT 10H
       INC CX
       INC DX
       DEC BX        	
    JNZ T1
    POP DX
    POP CX
    POP BX
    POP AX
	ret
DRAWTRAP endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawsnow proc far
   
    MOV SI,OFFSET LEN   
    MOV BX,[SI+2]
    MOV LENSTART,CX     ;SAVE CX VALUE FOR LATER LOOPS
    MOV DI,DX           ;SAVE DX VALUE FOR LATER LOOPS
    SUB CX,10           ;CX IS GIVEN AT MIDPOINT
    MOV AL,snowcolor          ;CHOSEN COLOUR
    MID1:               ;DRAW THE MIDDLE CROSS OF THE SNOW FLAKE
       INT 10H
       INC CX
       DEC BX        	
    JNZ MID1
    MOV BX,[SI+2]
    MOV CX,LENSTART
    SUB DX,10
    MID2:             
       INT 10H
       INC DX
       DEC BX        	
    JNZ MID2
    MOV DX,DI
    MOV BX,17
    SUB CX,8
    SUB DX,8
    X1:               ;DRAW THE MIDDLE CROSS WITH 45 DEGREE OF THE SNOW FLAKE
       INT 10H
       INC CX
       INC DX
       DEC BX        	
    JNZ X1
    MOV BX,17
    MOV CX,LENSTART
    MOV DX,DI
    SUB CX,8
    ADD DX,8
    X2:            
       INT 10H
       INC CX
       DEC DX
       DEC BX        	
    JNZ X2
    MOV DX,DI
    MOV CX,LENSTART
    MOV BX,4
    SUB CX,8
    SUB DX,4
    DH1:               ;DRAW THE MIDDLE CROSS OF THE SNOW FLAKE
       INT 10H
       INC CX
       DEC BX        	
    JNZ DH1
    MOV BX,4
    DV1:     
       DEC DX
       DEC BX         
       INT 10H            	
    JNZ DV1
    MOV DX,DI
    MOV CX,LENSTART
    MOV BX,4
    ADD CX,8
    SUB DX,4
;;;;;;;
    DH2:               ;DRAW THE MIDDLE CROSS OF THE SNOW FLAKE
       INT 10H
       DEC CX
       DEC BX        	
    JNZ DH2
    MOV BX,4
    DV2:     
       DEC DX
       DEC BX         
       INT 10H            	
    JNZ DV2
;;;;;;;;;;;;;
    MOV DX,DI
    MOV CX,LENSTART
    MOV BX,4
    ADD CX,8
    ADD DX,4
    DH3:               ;DRAW THE MIDDLE CROSS OF THE SNOW FLAKE
       INT 10H
       DEC CX
       DEC BX        	
    JNZ DH3
    MOV BX,4
    DV3:     
       INC DX
       DEC BX         
       INT 10H            	
    JNZ DV3
;;;;;;;;;;;;;;
    MOV DX,DI
    MOV CX,LENSTART
    MOV BX,4
    SUB CX,8
    ADD DX,4
    DH4:               ;DRAW THE MIDDLE CROSS OF THE SNOW FLAKE
       INT 10H
       INC CX
       DEC BX        	
    JNZ DH4
    MOV BX,4
    DV4:     
       INC DX
       DEC BX         
       INT 10H            	
    JNZ DV4 
ret 
drawsnow endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
music proc far 
mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, 4560        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 25          ; Pause for duration of note.
.pause1:
        mov     cx, 65535
.pause2:
        dec     cx
        jne     .pause2
        dec     bx
        jne     .pause1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al
ret
music endp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Traptom proc far
push ax
mov al,tomtraps
cmp al,1
jl Endtrap
mov bx,offset gridarr
add bx,celltom
mov cl,0
MOV ch,[bx]
cmp ch,cl
jz TomPutAtrap
call music 
jmp Endtrap
TomPutAtrap:
dec tomtraps
mov bx,offset gridarr
add bx,celltom
mov cl,9
mov [bx],cl
mov cx,xtomn
mov dx,ytomn
sub cx,13
sub dx,13
MOV SI,OFFSET Greycolor
mov bx,LEN
call drawcell
call drawtom
Endtrap:

pop ax
ret
Traptom endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tomturn proc far     
 IsW: 
    cmp ah,72
	jnz isS
	WPRESSED:  
	  mov cx,ytomn
	  sub cx,13
	  cmp cx,30
	  jle WEND
	  mov cx,celltom
	  sub cx,9
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je WEND
	  mov bx,xtomn
	  mov xtom,bx
	  mov bx,ytomn
	  mov ytom,bx
	  mov bx,celltom
	  mov celltomold,bx
      sub ytomn,29
	  sub celltom,9
	
	  
     WEND:  
	   mov cl,1
	   mov tomtest,cl
	  
       jmp exittom          
IsS:
       cmp ah,80
      
	   jnz isD
	  
	SPRESSED:
      mov cx,ytomn
	  add cx,13
	  cmp cx,172
	  jge SEND
	  mov cx,celltom
	  add cx,9
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     
	  cmp al,1
	  je SEND
	  
	  mov bx,xtomn
	  mov xtom,bx
	  mov bx,ytomn
	  mov ytom,bx
	  mov bx,celltom
	  mov celltomold,bx
      add ytomn,29
	  add celltom,9  
	  
	  
   
  SEND: 
      mov cl,1
	  mov tomtest,cl  
     
      jmp exittom
IsD:
    cmp ah,77
  
	   jnz isT
	DPRESSED:
      mov cx,xtomn
	  add cx,13
	  cmp cx,288
	  jge DEND
	  mov cx,celltom
	  add cx,1
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je DEND
	  
	  mov bx,xtomn
	  mov xtom,bx
	  mov bx,ytomn
	  mov ytom,bx
	  mov bx,celltom
	  mov celltomold,bx
      add xtomn,29
	  add celltom,1
	 
	
   DEND: 
      mov cl,1
	  mov tomtest,cl   
     
      jmp exittom
isT :
       cmp ah,15
	   jnz IsA
	TPRESSED:
	  call Traptom
     
      jmp exittom
IsA:
     cmp ah,75
	 
	    jz Apressed
	   	mov ah,thunderflag ;CLEAR KEYBOARD BUFFER IN CASE OF THUNDER MODE BEFORE GOING TO JERRY
		cmp ah,1
		jz exittom
	 
		jmp exittom
    Apressed:
      mov cx,xtomn
	  sub cx,13
	  cmp cx,30
	  jle AEND
	  mov cx,celltom
	  sub cx,1
	  mov ax,cx
      mov BX,offset gridarr
      XLAT     ;translate and put the result in AL
	  cmp al,1
	  je AEND
	  mov bx,xtomn
	  mov xtom,bx
	  mov bx,ytomn
	  mov ytom,bx
	  mov bx,celltom
	  mov celltomold,bx
      sub xtomn,29
	  dec celltom
	  
	  
   AEND: 
      mov cl,1
	  mov tomtest,cl   
     
exittom:	  
ret
tomturn endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRINTCOLON PROC
push ax
push bx
push cx
	MOV AH,9
	MOV BH,0FH
	MOV AL,58
	MOV BL,0FH
	MOV CX,1
	INT 10H
pop cx
pop bx
pop ax
RET
PRINTCOLON ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DRAW THE GAME OVER SCREEN PAUSE OVER SCORE THEN ANNOUNCE WINNER
GAMEOVER proc
        MOV AH,2CH
        INT 21H
        MOV currentsecond,DH
	Add currentsecond,5 
	mov al,currentsecond
	mov ah,0h
	mov bl,60
	div bl
	mov currentsecond,ah
        ;;;;;;;;;;;CLEAR THE SCREEN
        MOV AX,0600H 
        MOV BX,00
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;;;;;;;;;;SET CURSOR
        MOV AH,2
        MOV BH,0
        MOV DL,10
	MOV DH,3
        INT 10H  
        ;;;;;;;;;;;;;;;;;;;;;;
        mov di,offset done
        mov cx,16
        mov count,cx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        over:
        MOV AH,9
	MOV BH,0
	MOV AL,[DI]
	MOV CX,1
	MOV BL,04H
	INT 10H
	INC DI
        ;;UPDATE CURSOR
	MOV AH,2
	INC DL
	INT 10H
        ;;
        dec count
	jnz over
;;;;;;;;;;;;;;;;;;;;;;;
	mov ah,2
        MOV BH,0
        MOV DL,13
	MOV DH,6
        INT 10H
        mov al,1
        cmp winjerry,1
        jz jerrywon
	DISPLAYSTRING tomwin
	
        jmp score
jerrywon:
      DISPLAYSTRING jerrywin
score:
	mov ah,2
    MOV BH,0
    MOV DL,7
	MOV DH,7
    INT 10H	
	DISPLAYSTRING JERRY
	mov ah,2
    MOV BH,0
    MOV DL,20
	MOV DH,7
    INT 10H	
	DISPLAYSTRING TOM
	mov ah,2
    MOV BH,0
    MOV DL,7
	MOV DH,9
        INT 10H	
	MOV CX,45
	MOV DX,75
    call DRAWCHEESE 
        
	MOV DL,22
	call PRINTCOLON
	mov ah,2
        MOV BH,0
        MOV DL,8
	mov dh,9
    INT 10H	
	MOV AH,9
	MOV BH,0
    MOV AL,JERRYCHEESE
    ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
	mov ah,2
    MOV BH,0
    MOV DL,7
	MOV DH,12
    INT 10H	
	MOV AH,0CH
	MOV CX,45
	MOV DX,99
        call DRAWsnow 
     
         MOV CX,150  ;trap draw
	 MOV DX,80
         call DRAWtrap
	MOV DL,22
	call PRINTCOLON
	mov ah,2
        MOV BH,0
        MOV DL,8
	mov dh,12
         INT 10H	
	MOV AH,9
	MOV BH,0
        MOV AL,JERRYFREEZE
        ADD AL, 48D 
 	MOV CX,1
	MOV BL,0FH
	INT 10H
	 mov ah,2
         MOV BH,0
         MOV DL,20
       	 MOV DH,10
         INT 10H
	 call PRINTCOLON
	mov ah,2
         MOV BH,0
         MOV DL,22
       	 MOV DH,10
         INT 10H
	
	MOV AH,9
	MOV BH,0
        MOV AL,TOMTRAPS
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
todos:
     MOV AH,2CH
     INT 21H
	 cmp dh,currentsecond
	 jnz todos

   ret 
GAMEOVER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
playjerry proc  FAR
 
	   mov ax,celljerry
           mov BX,offset gridarr
           XLAT    
	   mov bx,offset gridarr
	checkeatcheese:
	   cmp al,5
	   jnz iswincheese
	   add jerrycheese,1
	   call cheesemusic
	   mov bx,offset gridarr
	   add bx,celljerry
	   mov [bx],dl
	  
	iswincheese:
	  mov bl,gamecheese
	  cmp bl,jerrycheese
	  jne checkthunder
	  add winjerry,1
	  jmp theend
	checkthunder:
	  cmp al,8
	  jne checkfreeze
	  inc thunderflag
          push bx
          MOV AH,2CH
          INT 21H
          MOV currentsecond,DH
	  Add currentsecond,5 
	  push ax
	  mov al,currentsecond
	  mov ah,0h
	  mov bl,60
	  div bl
	  mov currentsecond,ah
	  pop ax
	  pop bx
	  mov bx,offset gridarr
	  add bx,celljerry
	  mov [bx],dl
	
        checkfreeze :
	  cmp al,6
	  jne checkfreetrapjerry
	  add jerryfreeze,1
	  call clearcell
	  call drawcell 
	  call drawjerry 

	   mov bx,offset gridarr
	   add bx,celljerry
	   mov [bx],dl
	checkfreetrapjerry:
	   cmp al,7
	   jne checktrap
	   add jerrylifes,1 
	   mov bx,offset gridarr
	   add bx,celljerry
	   mov [bx],dl
	checktrap:
	  cmp al,9
	  jne istom
       checkfreetrap :
	  mov bl,jerrylifes
	  cmp bl,1
	  jg notlose
	  add losejerry,1
	  jmp theend
	notlose: 
	 sub jerrylifes,1
	 call clearcell
	 call drawcell 
	 call drawjerry 
	 mov bx,offset gridarr
	 add bx,celljerry
	 mov [bx],dl
	 jmp istom
	theend: 
      Add gameEnd,1
	  call music
	  jmp endprocjerry
            
	istom:
	  mov bx,celljerry
	  cmp bx,celltom
	  jne endprocjerry
	  add wintom,1
	  MOV SI,OFFSET GridWhiteColor
	  MOV BX,LEN
	  mov cx,xtom
	  mov dx,ytom
	  sub cx,13
	  sub dx,13
	  call drawcell
	  call drawtom
	  jmp theend
	 	  
endprocjerry:	  
ret 
playjerry ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
playtom proc far 
	mov dl,0
	mov ax,celltom
    mov BX,offset gridarr
    XLAT     
	mov bx,offset gridarr
	checktraptom :
	cmp al,2
	jnz isjerry
	add tomtraps,1
	add bx,celltom
	mov [bx],dl
	   
	
	isjerry:
	mov bx,celljerry
	cmp bx,celltom
	jne endproctom
	add wintom,1
	  
	jmp endtom
	
	endtom: 
	call music
    Add gameEnd,1 
endproctom:	 
    ret 
playtom endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UPDATESTATUS PROC 
push ax
push bx
push cx
push dx	
	MOV DL,4
	CALL SETCURSOR
	MOV AH,9
	MOV BH,0
        MOV AL,JERRYCHEESE
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
;;;;;;;;;;;;;;;;;;;;;
	MOV DL,9
	CALL SETCURSOR
	MOV AH,9
	MOV BH,0
        MOV AL,JERRYFREEZE
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
;;;;;;;;;;;;;;;;;;;;;;
	MOV DL,15
	CALL SETCURSOR
	MOV AH,9
	MOV BH,0
        MOV AL,JERRYLIFES
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	MOV DL,20
	CALL SETCURSOR
	MOV AH,9
	MOV BH,0
        MOV AL,TOMTRAPS
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MOV DL,22
	CALL SETCURSOR
        MOV AH,ThunderFlag
        CMP AH,1
        JZ PRINTTHUNDER
        MOV AX,player
        CMP AX,0
        JZ PRINTJERRY
        CMP AX,1
        JZ PRINTTOM
PRINTTHUNDER:
        DISPLAYSTRING THMODE
        DISPLAYSTRING SPACE
        JMP ENDUPDATE
PRINTJERRY:
	DISPLAYSTRING JERRY
        DISPLAYSTRING JERRYPLAYER
	JMP ENDUPDATE
PRINTTOM:
	DISPLAYSTRING TOM
        DISPLAYSTRING TOMPLAYER
ENDUPDATE:
	pop dx
	pop cx
	pop bx
	pop ax
RET
UPDATESTATUS ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
STATUSBAR MACRO 
push ax
push bx
push cx
push dx
	MOV DX,175
	MOV AL,0FH
	STATUSH:
            MOV CX,0    
            STATUSV:
            INT 10H
            INC CX
            CMP CX,320D       	
            JNZ STATUSV      
            INC DX
            CMP DX,177
        JNZ STATUSH 
;;;;;;;;;;;;;;;;
	
	MOV CX,12
	MOV DX,188
        call DRAWCHEESE 	
;;;;;;;;;;;;;;; 
       MOV AH,0CH	
	MOV CX,53
	MOV DX,188
        call DRAWSNOW 
;;;;;;;;;;;;;;;
        MOV CX,140
        MOV DX,185
        call DRAWTRAP
;;;;;;;;;;;;;;;
        MOV CX,98
        MOV DX,188
        call DRAWheart
;;;;;;;;;;;;;;;
	MOV DL,3
	CALL SETCURSOR
	CALL PRINTCOLON
	MOV DL,8
	CALL SETCURSOR
	CALL PRINTCOLON
	MOV DL,14
	CALL SETCURSOR 
	CALL PRINTCOLON 
;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	MOV DL,19
	CALL SETCURSOR 
	CALL PRINTCOLON 
	pop dx
	pop cx
	pop bx
	pop ax
ENDM STATUSBAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWBUTTON MACRO
     LOCAL H1
     LOCAL V1
     ADD DX,10
     MOV LENEND,DX
     INC LENEND
     SUB DX,20
     MOV AL,4
     MOV AH,0CH
     SUB CX,40
  H1:                        ;DRAWING TWO HORIZONTAL LINES
        INT 10H
        ADD DX,20
        INT 10H
        SUB DX,20
        INC CX     
        CMP CX,230     	
  JNZ H1
  V1:                      ;DRAWING TWO VERTICAL LINES
        INT 10H
        SUB CX,150
        INT 10H
        ADD CX,150
        INC DX     
        CMP DX,LENEND     	
  JNZ V1
ENDM DRAWBUTTON
SETCURSORXY PROC
    PUSH AX
	PUSH BX
	MOV BH,0
    MOV AH,2
    INT 10H
    POP BX
    POP AX
RET
SETCURSORXY ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWMENU MACRO
        LOCAL BUTTONS
        LOCAL NAME
	MOV DL,13
	MOV BH,0
        MOV AH,2
        MOV DH,3
        INT 10H
        
        MOV CX,150
        MOV DX,44
        CALL DRAWLOGO
   
        MOV BH,0
        MOV DL,11
	MOV DH,12
        INT 10H
	DISPLAYSTRING HOWTOPLAY
        MOV BH,0
        MOV DL,11
	MOV DH,15
        INT 10H
	DISPLAYSTRING PLAYNOW
        MOV BH,0
	MOV DL,11
	MOV DH,18
        INT 10H
	DISPLAYSTRING CHATMODE
        MOV BH,0
        MOV DL,11
	MOV DH,21
	MOV BX,3
        INT 10H
	DISPLAYSTRING EXITGAME
        MOV DX,95
        MOV BX,4
	BUTTONS:
	    MOV CX,120	  
	    DRAWBUTTON
	    ADD DX,15
            DEC BX
       JNZ BUTTONS   
ENDM DRAWMENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HOWTOP PROC FAR
 ;;;;;;;;;;;CLEAR THE SCREEN
        MOV AX,0600H 
        MOV BX,00
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;;;;;;;;;;
        MOV AH,2
        MOV DX,0100H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY1

        MOV AH,2
        MOV DX,0200H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY2
	MOV AH,2
        MOV DX,0300H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY3
	MOV AH,2
        MOV DX,0400H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY4
	MOV AH,2
        MOV DX,0600H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY5
	MOV AH,2
        MOV DX,0700H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY6
	MOV AH,2
        MOV DX,0800H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY7
	MOV AH,2
        MOV DX,0A00H
	MOV BH,0
        INT 10H
	DISPLAYSTRING HOWTOPLAY8
	
BACKMENU:
        MOV Ax,0h
	INT 16H 
	CMP AL,27D
	JE RE
	JMP BACKMENU          
RE:
    RET
HOWTOP  ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NOTIFICATION PROC
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
	MOV CL,INVITE
	

        MOV BX,0
	MOV AH,2
        MOV DH,22
	MOV DL,0
	INT 10H

        CMP CL,0
        JZ S1
        CMP CL,1
        JZ S2
        JMP S3
       
S1:
        DISPLAYSTRING RECEIVEchatINV
	JMP S3
S2:
         DISPLAYSTRING SENTCHATINV
        
	JMP S3
S3:
	MOV BX,0
	MOV AH,2
        MOV DH,23
	MOV DL,0
	INT 10H
        CMP Cl,4
	JZ S4
        CMP CL,3
        JZ S5
	JMP NOTIFYEND
S4:  
        DISPLAYSTRING SENTgameINV
        JMP NOTIFYEND
S5:
       DISPLAYSTRING RECEIVEgameINV
     
	JMP NOTIFYEND
NOTIFYEND:       

	POP DX
	POP CX
	POP BX
	POP AX
RET
NOTIFICATION ENDP
CLEARDOWN PROC
   mov ah,6       ; function 6
   mov al,1        ; scroll by 1 line    
   mov bh,7       ; normal video attribute         
   mov ch,11     ; upper left Y
   mov cl,0        ; upper left X
   mov dh,20      ; lower right Y
   mov dl,79      ; lower right X 
   int 10h           
RET
CLEARDOWN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CLEARUP PROC
   mov ah,6       ; function 6
   mov al,1        ; scroll by 1 line    
   mov bh,7       ; normal video attribute         
   mov ch,0       ; upper left Y
   mov cl,0        ; upper left X
   mov dh,9    ; lower right Y
   mov dl,79      ; lower right X 
   int 10h 
RET
CLEARUP ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SCROLLBACKu PROC
PUSH AX
PUSH BX
PUSH CX
PUSH DX
      CMP POSUPX,0
      JZ CHY
      
      DEC POSUPX
HANDLE:
      MOV DL,POSUPX
      MOV DH,POSUPY
      CALL SETCURSORXY
      MOV CL,20h
      MOV VALUE,CL
      CALL DISPLAYCHAR
      JMP ENDSCROLL
CHY:
      CMP POSUPY,0
      JZ ENDSCROLL
      MOV DL,79
      MOV POSUPX ,DL
      DEC POSUPY
      JMP HANDLE
ENDSCROLL:
POP DX
POP CX
POP BX
POP AX
RET
SCROLLBACKu ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SCROLLBACKd PROC
PUSH AX
PUSH BX
PUSH CX
PUSH DX
      CMP POSDOWNX,0
      JZ CHYD
      
      DEC POSDOWNX
HANDLED:
      MOV DL,POSDOWNX
      MOV DH,POSDOWNY
      CALL SETCURSORXY
      MOV CL,20h
      MOV VALUE,CL
      CALL DISPLAYCHAR
      JMP ENDSCROLLD
CHYD:
      CMP POSDOWNY,11
      JZ ENDSCROLLD
      MOV DL,79
      MOV POSDOWNX ,DL
      DEC POSDOWNY
      JMP HANDLED
ENDSCROLLD:
POP DX
POP CX
POP BX
POP AX
RET
SCROLLBACKd ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRINTDOWN PROC
        PUSH DX
        MOV DL,POSDOWNx
	MOV DH,POSDOWNy
	CALL SETCURSORXY
        MOV DH,VALUE
	CMP DH,08H
        JZ BACKSPACED	
        
        CMP DH,0DH
        JNZ NOTENTERDOWN
        JMP ENTERDOWN
NOTENTERDOWN:
	CALL DISPLAYCHAR
	INC POSDOWNx
        MOV DH,POSDOWNX
        CMP DH,80
        JNZ ENDPDOWN
ENTERDOWN:
        MOV DH,0
        MOV POSDOWNX,DH
        INC POSDOWNY
        MOV DH,POSDOWNY
        CMP DH,21
        JNZ ENDPDOWN
        DEC POSDOWNY
        CALL CLEARDOWN
BACKSPACED:
        CALL SCROLLBACKD
ENDPDOWN:
	POP DX
    RET 
PRINTDOWN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRINTUP PROC
    PUSH DX
    MOV DL,POSUPx
	MOV DH,POSUPy
	CALL SETCURSORXY 
        MOV DH,VALUE
        CMP DH,08H
	JZ BACKSPACEU
        CMP DH,0DH
        JNZ NOTENTERUP
        JMP ENTERUP
NOTENTERUP:
	CALL DISPLAYCHAR
	INC POSUPX
        MOV DH,POSUPX
        CMP DH,80
        JNZ ENDPUP
ENTERUP:
        MOV DH,0
        MOV POSUPX,DH
        INC POSUPY
        MOV DH,POSUPY
        CMP DH,10
        JNZ ENDPUP
        DEC POSUPY
        CALL CLEARUP
BACKSPACEU:
        CALL SCROLLBACKU
ENDPUP:
	POP DX
        RET
PRINTUP ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECKSEND PROC
          push ax
          push bx
          push cx
          push dx
        ;Check that Transmitter Holding Register is Empty
		mov dx , 3FDH		; Line Status Register
                In al , dx 			;Read Line Status
  	       	test al , 00100000b
                ;If empty put the VALUE in Transmit data register
		
		JZ ENDSEND
		
  		mov dx , 3F8H		; Transmit data register
  		mov  al,VALUE
  		out dx , al	
		
ENDSEND:
     
        pop dx
     pop cx
     pop bx
     pop ax
		RET
CHECKSEND ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECKREAD PROC
                push ax
                push bx
                push cx
                push dx
                ;Check that Data is Ready
		mov dx , 3FDH		; Line Status Register	
	        in al , dx 
  		test al , 1 ;Not Ready
		JZ ENDREAD
  		mov dx , 03F8H  ;If Ready read the VALUE in Receive data register
  		in al , dx 
                CMP AL,1BH
                jz endchatting
  		mov VALUE , al
		CALL PRINTDOWN
                jmp endread

endchatting:
      mov di,1
ENDREAD:
               pop dx
               pop cx
               pop bx
               pop ax
	
      RET
CHECKREAD ENDP
splitscreen proc 
    PUSH DX
	MOV BX,0
    MOV AH,2        ;MOVE CURSOR
    MOV DX,0068H
    INT 10H
    ;;;;;;;;;;;;;;;CLEAR SCREEN
    MOV AX,0600H
    MOV BH,07
    MOV CX,0
    MOV DX,184FH
    INT 10H
    ;;;;;;;;;;;;;  
	MOV DH,10
	MOV DL,0
	CALL SETCURSORXY
    DISPLAYSTRING SPLIT	
    MOV DH,21
	MOV DL,0
	CALL SETCURSORXY
    DISPLAYSTRING SPLIT	
    POP DX      
    RET
splitscreen endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHATSCREEN  PROC
PUSH AX
PUSH BX
PUSH CX
PUSH DX
push di
             ;Change to Text MODE
                 MOV AH,0          
                 MOV AL,03h
                 INT 10h 
               MOV AH,0
               MOV POSUPX,AH
               MOV POSDOWNX,AH
               MOV POSUPY,AH
               MOV ACCEPTCHAT,AH
               MOV AH,2
               MOV INVITE,AH
               MOV AH,11
               MOV POSDOWNY,AH
               
                 CALL SPLITSCREEN
		 CHATNOW:
		   MOV Ah,1h
	           INT 16H
                   jz receive 
                   MOV VALUE,AL
		   CALL CHECKSEND  
                   CMP AL,1BH		;ascii of esc   
		   JZ ENDCHAT       
                   CALL PRINTUP
                    mov ax,0
		    int 16h
                   receive:  
                   mov di,0
		   CALL CHECKREAD 
                   cmp di,0
		   Jz CHATNOW
ENDCHAT:
  mov ah,0                      ;resetting up and down cursor position
  mov posdownx,ah
  mov posupx,ah
  mov posupy,ah
  mov ah,11
  mov posdowny,ah
mov ah,0			; change to video mode
	mov al,13h
	int 10h
pop di
POP DX
POP CX
POP BX
POP AX
RET
CHATSCREEN ENDP
chooselevel PROC
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
lev:
        ;Change to Text MODE
        MOV AH,0          
        MOV AL,03h
        INT 10h 
      
        MOV BX,0
        MOV AH,2        ;MOVE CURSOR
        MOV Dx,06H
        INT 10H
        ;;;;;;;;;;;;;;;CLEAR SCREEN
        MOV AX,0600H
        MOV BH,07
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;  
        DISPLAYSTRING WHICHlevel
        
	MOV Ax,0h
	INT 16H
	CMP AL,31H
	JE lev1
	CMP AL,32h
	JE lev2
	jmp lev
lev1:
        mov al,1
        mov level,al
        jmp endlev
lev2:
	mov al,2
        mov level,al
;;;;;;;;
endlev:
        POP DX
	POP CX
	POP BX
	POP AX
RET
chooselevel ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FIRSTPLAYER PROC
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
		mov ah,0
		mov gamemode,ah
        call chooselevel
        
      
        POP DX
	POP CX
	POP BX
	POP AX	
RET
FIRSTPLAYER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INVITEOUT PROC 
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
		
		   MOV AL,1
                   CMP AL,INVITE
                   JZ SENDZEROC
                   mov al,4
                   CMP AL,INVITE
                   jz sendthree
	           JMP OUTC		   
SENDZEROC:
		   ;;SENING MY INVITATIONS
                   MOV VALUE,0
		   CALL CHECKSEND
		   jmp outc2      
SENDthree:
		   ;;SENING MY INVITATIONS
                   MOV VALUE,3
		   CALL CHECKSEND
		   jmp outc2               
OUTC:	
		   MOV AL,INVITE
          	   MOV VALUE,AL
		   CALL CHECKSEND     
outc2:
        POP DX
	POP CX
	POP BX
	POP AX
RET
INVITEOUT ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INVITEIN PROC
           PUSH AX
	   PUSH BX
	   PUSH CX
	   PUSH DX

                   MOV AL,2           ;;CHECK IF I RECEIVED ANY INVITATIONS
                   MOV VALUE,AL                   
		   CALL CHECKREAD 
	           MOV AL,VALUE  
	           CMP AL,5
                   JZ Fchat
		   cmp al,6
		   jz fplay	
   		   CMP AL,0           ;;IF I RECEIVED A 0 THEN I HAVE A CHAT INVITATION
                   jz chatinv
                   cmp al,3
                   jz gameinv
                  
                  jmp ENDCHECKINV
gameinv:            
       MOV INVITE,AL
       jmp ENDCHECKINV
chatinv:            
       MOV INVITE,AL
       jmp ENDCHECKINV
Fchat:
     
      mov al,1
      mov acceptchat,al
       jmp ENDCHECKINV
FPLay:
      call firstplayer
      mov al,1
      mov accepted,al
      mov al,50
      mov value,al
      call checksend

ENDCHECKINV:
           
	POP DX
	POP CX
	POP BX
	POP AX	
RET
INVITEIN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
waitflag proc
	        push ax
                push bx
                push cx
                push dx
                ;Check that Data is Ready
               
    
       

         MOV AL,1
        MOV GAMEMODE,1
        CALL CHECKREAD ;;READING THE GAME MODE WHO IS PLAYING
        MOV AL,VALUE
        MOV LEVEL,AL
;LOOPLEVEL:
       ; CALL CHECKREAD ;;READING THE GAME MODE WHO IS PLAYING
       ; MOV AL,VALUE
       ; CMP AL,'A'
       ; JZ GETL 
      ;  MOV LEVEL,AL
        ;JMP LOOPLEVEL
;GETL:       


ENDREADw:
		mov dx , 3FDH		; Line Status Register	
	        in al , dx 
  		test al , 1 ;Not Ready
		JZ ENDREADw
  		mov dx , 03F8H  ;If Ready read the VALUE in Receive data register
           
  		in al , dx 
             
                CMP AL,50
                jnz endreadw


ENDWAIT:
               pop dx
               pop cx
               pop bx
               pop ax
ret
waitflag endp
MENUPAGE PROC
 
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
       

        
RETOMENU:
 
        ;;;;;;;;;;;CLEAR THE SCREEN
         
	mov ah,0			; change to video mode
	mov al,13h
	int 10h
      
        MOV AX,0600H 
        MOV BX,00
        MOV CX,0
        MOV DX,184FH
        INT 10H
       

        ;;;;;;;;;;;;;;;;;;;;;;
	DRAWMENU
	
        JMP BACKAGAIN  ;;;;;;;;;;;;;;GET THE KEYSTROKE
WAITFOR:
        
	CALL INVITEIN
	;CALL INVITEINC
        mov ah,1
        cmp accepted,ah
	jz top2    
        cmp acceptchat,ah
	jz achat
        CALL NOTIFICATION
	MOV AH,1
	INT 16H 
	JZ WAITFOR

	MOV Ax,0h      ;;FLUSH KEYBOARD
	INT 16H

	CMP AL,31H
	JE HOWTO
	CMP AL,32H
	JE TOPLAY
	CMP AL,33H
	JE CHATTING 
	CMP AL,34H
	JE DOSBOX
BACKAGAIN:
	JMP WAITFOR
	
DOSBOX:
	MOV AH,4CH
	INT 21H
HOWTO:
	CALL HOWTOP
	JMP RETOMENU
CHATTING:
	
	MOV BL,0
	CMP INVITE,BL
        JZ BEGINCHAT
	
        jmp temp          ;;temporary jump
top2:
     jmp top	
temp:
	MOV BL,1
	MOV INVITE,BL
        CALL INVITEOUT
	;CALL INVITEINC
        JMP WAITFOR
BEGINCHAT:
        MOV BL,5
	MOV INVITE,BL
        CALL INVITEOUT
	;CALL INVITEINC
achat:	                         ;;;the other player accepted my invitation
        CALL CHATSCREEN
        JMP RETOMENU
TOPLAY: 
	MOV BL,3
	CMP INVITE,BL
	JZ SECONDLY
	
	MOV BL,4
	MOV INVITE,BL
	CALL INVITEOUT
	;CALL INVITEING
	JMP WAITFOR
SECONDLY:  
        MOV BL,6
	MOV INVITE,BL
        CALL INVITEOUT
	;CALL INVITEING
        call waitflag
top:        
        POP DX
	POP CX
	POP BX
	POP AX
	RET
MENUPAGE ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RESETVAR PROC
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
       MOV AL,1
       MOV jerrylifes,AL
       MOV testjerry,AL
       MOV tomtest,AL
       MOV AL,0
       MOV jerrycheese,AL
       MOV tomtraps,AL
       MOV jerryfreeze,AL
       Mov gameEnd,AL
       mov ax,0
       mov countgrid,Ax
       mov endescape,AL
       MOV losejerry,AL
       MOV winjerry,AL
       MOV ThunderFlag,AL
       MOV Freezecounter,AL
       MOV  wintom,AL
       MOV AL,4
       MOV gamecheese,AL
       MOV AX,43
       MOV XX,AX
       MOV YY,AX
       MOV AX,44
       MOV celltom,AX
       MOV celltomold,AX
       MOV AX,0
       MOV celljerry,AX
       MOV celljerryold,AX
       MOV PLAYER,AX
       MOV AX,275
       MOV xtom,AX
       MOV AX,159
       MOV ytom,AX
       MOV CX ,45
     
       MOV DI,OFFSET gridarr
       RESTORE:
             MOV AL,0
             MOV [DI],AL
             INC DI
       LOOP RESTORE
	   MOV DI,OFFSET GRIDCOLOR
	   MOV CX ,45
       RESTORECOLOR:
             MOV AL,0FH
             MOV [DI],AL
             INC DI
       LOOP RESTORECOLOR

        POP DX
	POP CX
	POP BX
	POP AX
	RET
RESETVAR ENDP
DRAWMENU macro 
        push ax
	push bx
	push cx
	push dx
	MOV DL,13
	MOV BH,0
        MOV AH,2
        MOV DH,3
        INT 10H
        
        MOV CX,150
        MOV DX,33
        CALL DRAWLOGO
   
        MOV BH,0
        MOV DL,11
	MOV DH,9
        INT 10H
	DISPLAYSTRING HOWTOPLAY
        MOV BH,0
        MOV DL,11
	MOV DH,12
        INT 10H
	DISPLAYSTRING PLAYNOW
        MOV BH,0
	MOV DL,11
	MOV DH,15
        INT 10H
	DISPLAYSTRING CHATMODE
        MOV BH,0
        MOV DL,11
	MOV DH,19
	MOV BX,3
        INT 10H
	DISPLAYSTRING EXITGAME
        MOV BH,0
        MOV DL,0
	MOV DH,21
	MOV BX,3
        INT 10H
	DISPLAYSTRING split2
        
        MOV DX,75
        MOV BX,4
	BUTTONS:
	    MOV CX,120	  
	    DRAWBUTTON
	    ADD DX,15
            DEC BX
       JNZ BUTTONS   
	pop dx
	pop cx
	pop bx
	pop ax

ENdm DRAWMENU 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-----------------------------
Readdata proc near
	mov dx , 3FDH		; Line Status Register
	CHK:in al , dx 
  		test al , 1
  		JZ exitreadtemp     ;Not Ready
 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		mov ReadValue , al
		cmp al,1bh
		jz requestendtemp
		mov ah,readvalue
		call tomturn
	MOV BX,celltomold   ;check if tom previous cell is jerry then don't clear
	CMP BX,celljerry
	jz outofrangeread
	  mov cx,xtom         ;clear by drawing a cell after checking if it's a white cell or grey cell
	mov dx,ytom
	sub dx,13
	sub cx,13
	mov bx,offset gridarr
	add bx,celltomold
	push cx
	mov cl,9
	cmp [bx],cl
	jne conttrapread
	pop cx
	MOV SI,OFFSET Greycolor
	JMP CONTCELLread
	requestendtemp:
	jmp requestend
	conttrapread:
	pop cx
	MOV SI,OFFSET GridWhiteColor
	CONTCELLread:
   	MOV BX,LEN
	call drawcell
	checkcheestomread:         ;check if tom previous cell is cheese then draw it again after clearing
	mov si,offset gridarr 
    add si,celltomold
    mov cl,[si]	  
	cmp cl,5
	jnz checkfreetomread
	MOV CX,xtom
    MOV DX,ytom
    call  DRAWCHEESE 
	jmp checkfreetomread
	exitreadtemp:
	jmp exitread
	checkfreetomread:  ;check if tom previous cell is Remove trap then draw it again after clearing
	mov si,offset gridarr  ;check freetom
    add si,celltomold
    mov cl,[si]	  
	cmp cl,7
	jnz outofrangeread
	MOV CX,xtom
    MOV DX,ytom
    call  DRAWheart
outofrangeread:
	call  drawtom
	call playtom	 ;call playjerry procedure which handles jerry weapons based on his position
	CALL UPDATESTATUS
	  add readtest,1
		jmp exitread
		requestend:
		mov endrequest,1
	 
	exitread:	
ret
Readdata endp
;------------------------------
Readdatatom proc near
	mov dx , 3FDH		; Line Status Register
	CHKtom:in al , dx 
  		test al , 1
  		JZ exitread     ;Not Ready
 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
  		in al , dx 
  		mov ReadValue , al
		cmp al,1bh
		jz requestendtom
		mov ah,readvalue
		call jerryturn
	MOV BX,celltom   ;check if tom is in jerry previous cell then don't clear
	CMP BX,celljerryold
	      jz drwread
		 mov cx,xx       ;clear by drawing the white cell 
	 mov dx,yy
	 sub dx,13
	 sub cx,13
	 MOV SI,OFFSET GridWhiteColor
	 MOV BX,LEN
	 call drawcell
	 trapjerryread:           ;check if jerry previous cell is trap then draw the trap weapon again after clearing cuz it's tom waepon not jerry
	 mov si,offset gridarr  
     add si,celljerryold
     mov cl,[si]	  
	 cmp cl,2
	 jnz drwread
	 MOV CX,xx
     MOV DX,yy
     call  DRAWTRAP 
	 jmp drwread
	 
	drwread:
      call  drawjerry
	  call playjerry	 ;call playjerry procedure which handles jerry weapons based on his position
	  CALL UPDATESTATUS
	  add readtest,1
		jmp exitreadtom
		requestendtom:
		mov endrequest,1
	 
	exitreadtom:	
ret
Readdatatom endp
;-------------------------------
Senddata proc near
mov dx , 3FDH		; Line Status Register
AGAIN:  In al , dx 			;Read Line Status
  		test al , 00100000b
  		JZ Exitsend   
        mov dx , 3F8H		; Transmit data register
  		mov  al,SendValue
  		out dx , al
  
 Exitsend:
 
ret
Senddata endp
;------------------------------
clrlowerScreen proc near
push ax
push bx
push cx
push dx
mov ah,06
mov al,1         ;scroll one line up
mov bh,70h       ;attribute of lower screen
mov ch,13        ;upper left coordinates of lower screen(0,13)
mov cl,0         
mov dh,24        ;lower right coordinates of lower screen(79,24)
mov dl,79
int 10h
pop dx
pop cx
pop bx
pop ax
ret
clrlowerScreen endp
;----------------------------
SetcursorFo2 proc near
push ax
push bx
push cx
push dx

mov dl,prevxup
mov dh,prevyup

cmp dl,80
jnz contsetfo2
inc prevyup
mov dh,prevyup
cmp dh,13
jne dontclearup
call clrupperScreen
mov dl,0
dec dh
mov prevxup,dl
mov prevyup,dh
dontclearup:
mov dl,0
mov prevxup,dl
contsetfo2:
inc prevxup
mov ah,2
mov bh,0h
int 10h
pop dx
pop cx
pop bx
pop ax
ret
SetcursorFo2 endp  
;------------------------------------
SetcursorT7t proc near
push ax
push bx
push cx
push dx
mov dl,prevxdown
mov dh,prevydown

cmp dl,80
jnz contsett7t
inc prevydown
mov dh,prevydown
cmp dh,25
jne dontcleardown
call clrlowerScreen
mov dl,0
dec dh
mov prevxdown,dl
mov prevydown,dh
dontcleardown:
mov dl,0
mov prevxdown,dl
contsett7t:
inc prevxdown
mov ah,2
mov bh,0h
int 10h

pop dx
pop cx
pop bx
pop ax
ret
SetcursorT7t endp
;---------------------------------
PrintSenddata proc near
push ax
push bx
push cx
push dx
call SetcursorFo2

mov ah,9h ;Display
mov bh,0
mov al,	SendValue 
mov cx,1 ;1 time
mov bl,00110000b 
int 10h
pop dx
pop cx
pop bx
pop ax
ret
 PrintSenddata endp 
;-----------------------------
PrintReaddata proc near
push ax
push bx
push cx
push dx
call SetcursorT7t
mov ah,9h ;Display
mov bh,0
mov al,ReadValue;Letter D
mov cx,1 ;1 times
mov bl,70h 
int 10h
pop dx
pop cx
pop bx
pop ax
ret
 PrintReaddata endp
;---------------------------------- 
clrupperScreen PROC near
push ax
push bx
push cx
push dx
	
mov ah,06
mov al,1     
mov bh,30h   ;Attribute of upper screen

mov ch,0   ;upper left coordinates of upper screen(0,0)
mov cl,0

mov dh,12  ;lower right coordinates of upper screen(79,12)
mov dl,79
int 10h		

pop dx
pop cx
pop bx
pop ax
ret
clrupperScreen ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
READSTRING MACRO INMSG
   local done
   local read
   mov cx,0
   LEA SI,INMSG
   READ:
         MOV AH,01H
         INT 21H
         CMP AL,13
         JE DONE
         CMP CX,15
         JE DONE
         MOV [SI],AL
         INC SI	
	 INC CX
         JMP READ
     DONE:
        
ENDM READSTRING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHECK USERNAME BEGIN WITH A LETTER
CHECKINITIAL MACRO TOCHECK
         local pass
         local endcheck
          PUSH AX
          PUSH SI
          LEA SI,TOCHECK
          MOV AL,[SI]
          CMP AL,41H   ;;A CAPITAL
          JL ENDCHECK
          CMP AL,5AH
          JLE PASS     ;; Z CAPITAL CHECK IF IN RANGE
          CMP AL,61H   ;;A SMALL
          JL ENDCHECK
          CMP AL,7AH
          JLE PASS
          JMP ENDCHECK 
PASS:
   MOV DI,1
ENDCHECK:
   POP SI
   POP AX
ENDM CHECKINITIAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;IF STRING INVALID MAKE IT AN EMPTY STRING AGAIN
FLUSHSTRING proc
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
     
       mov cx,10
       F:
         MOV AL,' '
         MOV [SI],AL
         INC SI	
	 DEC CX
         JNZ F
     
        POP DX
	POP CX
	POP BX
	POP AX
	RET
FLUSHSTRING ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;USERNAME2 SCREEN TO ENTER NAME
Username2 PROC
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
TAKEUSER2:
        LEA SI,tomPLAYER
        CALL FLUSHSTRING
        MOV BX,0
        MOV AH,2        ;MOVE CURSOR
        MOV DX,0068H
        INT 10H
        ;;;;;;;;;;;;;;;CLEAR SCREEN
        MOV AX,0600H
        MOV BH,07
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;  
        DISPLAYSTRING inTOM
        READSTRING TOMPLAYER
        MOV DI,0
        CHECKINITIAL TOMPLAYER
        CMP DI,0
        JE TAKEUSER2
        POP DX
	POP CX
	POP BX
	POP AX
	RET
Username2 ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;USERNAME1 SCREEN TO ENTER NAME
Username1 PROC
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
TAKEUSER1:
        LEA SI,JERRYPLAYER
        CALL FLUSHSTRING
        ;Change to Text MODE
        MOV AH,0          
        MOV AL,03h
        INT 10h 
      
        MOV BX,0
        MOV AH,2        ;MOVE CURSOR
        MOV DX,0068H
        INT 10H
        ;;;;;;;;;;;;;;;CLEAR SCREEN
        MOV AX,0600H
        MOV BH,07
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;  
        DISPLAYSTRING injerry
        READSTRING JERRYPLAYER
        MOV DI,0
        CHECKINITIAL JERRYPLAYER
        CMP DI,0
        JE TAKEUSER1 
        CALL USERNAME2
        POP DX
	POP CX
	POP BX
	POP AX
	RET
Username1 ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Username PROC
        PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
TAKEUSER:
        LEA SI,JERRYPLAYER
        CALL FLUSHSTRING
        ;Change to Text MODE
        MOV AH,0          
        MOV AL,03h
        INT 10h 
      
        MOV BX,0
        MOV AH,2        ;MOVE CURSOR
        MOV DX,0068H
        INT 10H
        ;;;;;;;;;;;;;;;CLEAR SCREEN
        MOV AX,0600H
        MOV BH,07
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;  
        DISPLAYSTRING inname
        READSTRING myname
        MOV DI,0
        CHECKINITIAL myname
        CMP DI,0
        JE TAKEUSER
        POP DX
	POP CX
	POP BX
	POP AX
	RET
Username ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PAUSE THE SCREEN WITH SCORES THEN TO MAIN MENU
EndEscapegame proc far 
         MOV AH,2CH
        INT 21H
        MOV currentsecond,DH
	Add currentsecond,5 
	mov al,currentsecond
	mov ah,0h
	mov bl,60
	div bl
	mov currentsecond,ah
        ;;;;;;;;;;;CLEAR THE SCREEN
        MOV AX,0600H 
        MOV BX,00
        MOV CX,0
        MOV DX,184FH
        INT 10H
        ;;;;;;;;;;;;;;;;;;;;;;SET CURSOR
        MOV AH,2
        MOV BH,0
        MOV DL,10
	MOV DH,3
        INT 10H  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scoreEscape:
	mov ah,2
        MOV BH,0
        MOV DL,7
	MOV DH,7
        INT 10H	
	DISPLAYSTRING JERRY
	mov ah,2
        MOV BH,0
        MOV DL,20
	MOV DH,7
        INT 10H	
	DISPLAYSTRING TOM
	mov ah,2
        MOV BH,0
        MOV DL,7
	MOV DH,9
        INT 10H	
	MOV CX,45
	MOV DX,75
        call DRAWCHEESE 
	MOV DL,22
	call PRINTCOLON
	mov ah,2
        MOV BH,0
        MOV DL,8
	mov dh,9
        INT 10H	
	MOV AH,9
	MOV BH,0
        MOV AL,JERRYCHEESE
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
	mov ah,2
        MOV BH,0
        MOV DL,7
	MOV DH,12
        INT 10H	
	MOV AH,0CH
	MOV CX,45
	MOV DX,99
        call DRAWSNOW 
        MOV CX,150  ;trap draw
	MOV DX,80
        call DRAWtrap
	MOV DL,22
	call PRINTCOLON
	mov ah,2
        MOV BH,0
        MOV DL,8
	mov dh,12
        INT 10H	
	MOV AH,9
	MOV BH,0
        MOV AL,JERRYFREEZE
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
	mov ah,2
        MOV BH,0
        MOV DL,20
	MOV DH,10
        INT 10H
	call PRINTCOLON
	mov ah,2
        MOV BH,0
        MOV DL,22
       	MOV DH,10
        INT 10H
	
	MOV AH,9
	MOV BH,0
        MOV AL,TOMTRAPS
        ADD AL, 48D 
	MOV CX,1
	MOV BL,0FH
	INT 10H
todosEscape:
     MOV AH,2CH
     INT 21H
	 cmp dh,currentsecond
	 jnz todosEscape

ret
EndEscapegame endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawgridarr proc far
push ax
push bx
push cx
push dx
mov cx,0
mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawSnowgrid:
push cx

mov al,[di]
cmp al,6
jne endgridsnow
mov ah,0ch
mov cx,countgrid
mov cell,cx
call calcxy
MOV CX,x
MOV DX,y
call drawsnow
endgridsnow:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawSnowgrid


mov cx,0
mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawtrapgrid:
push cx
mov cx,countgrid
mov cell,cx
call calcxy
gridtrap:
mov al,[di]
cmp al,2
jne endgrid
MOV CX,x
MOV DX,y
call drawtrap
endgrid:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawtrapgrid
mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawcheesegrid:
push cx
mov cx,countgrid
mov cell,cx
call calcxy

mov al,[di]
cmp al,5
jne endgridcheese
MOV CX,x
MOV DX,y
call drawcheese
endgridcheese:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawcheesegrid

mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawThundergrid:
push cx
mov cx,countgrid
mov cell,cx
call calcxy

mov al,[di]
cmp al,8
jne endgridthunder
MOV CX,x
MOV DX,y
call drawthunder
endgridthunder:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawThundergrid

mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawFreegrid:
push cx
mov cx,countgrid
mov cell,cx
call calcxy

mov al,[di]
cmp al,7
jne endgridheart
MOV CX,x
MOV DX,y
call drawheart
jmp endgridheart

endgridheart:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawFreegrid

mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawTomgrid:
push cx
mov cx,countgrid
mov cell,cx
call calcxy

mov al,[di]
cmp al,4
jne endgridtom
mov cx,countgrid
mov celltom,cx
jmp endgridtom

endgridtom:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawTomgrid

mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawjerrygrid:
push cx
mov cx,countgrid
mov cell,cx
call calcxy

mov al,[di]
cmp al,3
jne endgridjerry
mov cx,countgrid
mov celljerry,cx
jmp endgridjerry

endgridjerry:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawjerrygrid

mov cx,celljerry
mov celljerryold,cx
mov cell,cx
call calcxy
mov cx,x
mov xnew,cx
mov xx,cx
mov cx,y
mov ynew,cx
mov yy,cx
call drawjerry

mov cx,celltom
mov celltomold,cx
mov cell,cx
call calcxy
mov cx,x
mov xtomn,cx
mov xtom,cx
mov cx,y
mov ytomn,cx
mov ytom,cx
call drawtom
pop dx
pop cx
pop bx
pop ax
ret 
drawgridarr endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DRAWBLACK PROC FAR
push ax
push bx
push cx
push dx
mov cx,0
mov countgrid,cx
mov cx,45
mov di,offset gridarr

drawblackgrid:
push cx

mov al,[di]
cmp al,1
jne endblack
mov cx,countgrid
mov bx,offset GRIDCOLOR
add bx,cx
mov al,8
mov [bx],al
endblack:
add countgrid,1
mov di,offset gridarr
add di,countgrid
pop cx
dec cx
cmp cx,0
jnz drawblackgrid
pop dx
pop cx
pop bx
pop ax
RET
DRAWBLACK ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
random proc far

   MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      

   mov  ax, dx
   xor  dx, dx
   mov  cx,40 
   div  cx       ; here dx contains the remainder of the division - from 0 to 44
   mov randnum,dl
  

ret
random endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TAKE THE RANDOM NUMBER THEN MAP THIS RANDOMIZATION TO ALL GRID ELEMENTS
randgridarr proc far
mov di,offset randomarr
mov cx,17
randgrid:
mov al,[di]
add al,randnum
mov ah,0
mov bx,45
div bl
mov [di],ah
inc di
loop randgrid

ret
randgridarr endp
creategridarr proc far
mov si,offset randomarr
cmp level,1
jnz level2weapons
mov bx,offset weapons
jmp begin
level2weapons:
mov bx,offset weaponslevel2
begin:
mov cx,17
grid:
mov di,offset gridarr
mov al,[si]
mov ah,0
add di,ax
mov al,[bx]
mov [di],al
inc si
inc bx
loop grid

ret
creategridarr endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UART
UART proc far
mov dx,3fbh 			; Line Control Register
mov al,10000000b		;Set Divisor Latch Access Bit
out dx,al				;Out it

mov dx,3f8h			
mov al,01h			
out dx,al

mov dx,3f9h
mov al,0
out dx,al


mov dx,3fbh
mov al,00011011b
; •	0:Access to Receiver buffer, Transmitter buffer
; •	0:Set Break disabled
; •	011:Even Parity
; •	0:One Stop Bit
; •	11:8bits
out dx,al

                           
ret
UART endp
; Readdata proc far
; push ax
; push bx
; push cx
; push dx
; mov dx , 3FDH		; Line Status Register
	; CHK:in al , dx 
  		; test al , 1
  		; JZ exitread                                    ;Not Ready
 ; ;If Ready read the VALUE in Receive data register
 
  	 ; mov dx , 03F8H
  	 ; in al , dx 
  	 ; mov ReadValue , al
	 ; mov ah,readvalue
	; call jerryturnrecieve

	 

	 
	; exitread:	
	 

; pop dx
; pop cx
; pop bx
; pop ax
; ret
; Readdata endp

; Senddata proc far
; push ax
; push bx
; push cx
; push dx
; mov dx , 3FDH		; Line Status Register
; AGAIN:  In al , dx 			;Read Line Status
  		; test al , 00100000b
  		; JZ Exitsend   
        ; mov dx , 3F8H		; Transmit data register
  		; mov  al,SendValue
  		; out dx , al

 ; Exitsend:
 ; pop dx
; pop cx
; pop bx
; pop ax
; ret
; Senddata endp
MAIN PROC FAR               
        MOV AX,@DATA
        MOV DS,AX 
	call uart
	call Username
	mov ah,0			; change to video mode
	mov al,13h
	int 10h
	
 GameEndCheck:			;Label to check if the game is over or one of the players Exits
        mov ah,gameEnd      ;gameEnd variable is 0 if the game isnot over and 1 if one of the players loses
        cmp ah,1
        jnz GameEscapeCheck
        call gameover      ; gameover procedure to display the gameover scene
	GameEscapeCheck:      ;Label to check if one of the users Exits the game
	mov ah,EndEscape
	cmp ah,1
        jnz Stgame        ; jump to start the game if both checks are false
	call EndEscapegame  ; EndEscapegame procedure to display the EndEscape scene 
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
Stgame:					;Start of the game
    	CALL MENUPAGE   ;Display the main menu
		 mov ah,0			; change to video mode
		 mov al,13h
		 int 10h
        MOV ACCEPTED,AH
        MOV AH,2
        MOV INVITE,AH
        GAMEBEGIN:       ;if the players choose to play start the game from here
        CALL RESETVAR    ; Reset the whole variables of the game at the begining
        ;call random      ;Random procedure to get a Random number
        call randgridarr  ;randgridarr procedure to random the grid array based on the random number
	call creategridarr ;creategridarr procedure to create the grid array (array descripes each cell in the grid)
	call DRAWBLACK    ;Fill the Color array of the grid with the right black and white colors
		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;;Start Drawing th Grid;;
	mov cx,0           
	mov countgrid,cx
        MOV AX,0600H
        MOV BH,00
        MOV CX,0
        MOV DX,184FH
        INT 10H
	MOV SI,OFFSET GRIDCOLOR  ;Init the si with gridcolor array
	MOV BX,LEN               ;init bx with the length of the grid
        MOV DX,30            ;init the first cell with x=30 and y=30
        MOV CO_Y,DX
        MOV CX,30
        MOV CO_X,CX
  GRIDY: 
    MOV CX,CO_X 
     GRIDX:
       MOV BX,LEN      
      call DRAWCELL
       ADD CX,3
       MOV DX,CO_Y
       CMP CX,280
       JL GRIDX
    MOV BX,29
    ADD CO_Y,BX
    MOV DX,CO_Y
    CMP DX,150D
    JL GRIDY
	;;End Drawing th Grid;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;Draw the StatusBar;;	
    STATUSBAR
	CALL UPDATESTATUS   ;Update the status bar with the initial values
;;End Draw the StatusBar;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

    call drawgridarr  ;Fill the grid with the characters and weapons (cheese,trap,freeze,free trap,thunder)
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  			  

;call split	   ;Split the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

cmp  gamemode,0
jnz disptomtemp
jmp disp
EndgameFlag1:
jmp GameEndCheck
disp:	
		
		mov dl,0            ;check if jerry change his position
		mov readtest,dl
		mov bx,xx
		cmp bx,xnew
		jne clear
		mov bx,yy
		cmp bx,ynew
		je donothing      ;if he doesn't change his position then don't clear anything and continue to do nothing
		jmp clear 
clear:	            ;start clearing the previous cell of jerry
	mov bl,testjerry 
	cmp bl,0
	jz drw
	MOV BX,celltom   ;check if tom is in jerry previous cell then don't clear
	CMP BX,celljerryold
	jz drw
	 mov cx,xx       ;clear by drawing the white cell 
	 mov dx,yy
	 sub dx,13
	 sub cx,13
	 MOV SI,OFFSET GridWhiteColor
	 MOV BX,LEN
	 call drawcell
	 jmp trapjerry
	 EndgameFlag1t:
	 jmp EndgameFlag1
;;;;;;;;;;;;;;;;;;;;;;;;;;;	
trapjerry:           ;check if jerry previous cell is trap then draw the trap weapon again after clearing cuz it's tom waepon not jerry
	 mov si,offset gridarr  
     add si,celljerryold
     mov cl,[si]	  
	 cmp cl,2
	 jnz compjerry
	 MOV CX,xx
     MOV DX,yy
     call  DRAWTRAP 
	 jmp compjerry
	disptomtemp:
jmp disptom	
;;;;;;;;;;;;;;;;;;;;;;;;;;;
compjerry:          ;continue jerry functionality 
    mov bl,0
	mov testjerry,bl
	drw:	        ; start drawing jerry
	call drawjerry 
	donothing:	
	call playjerry	 ;call playjerry procedure which handles jerry weapons based on his position
EndT3:
	cmp gameEnd,1
	jz EndgameFlag1t
	CALL UPDATESTATUS ;update status bar based on what weapons jerry takes during his turn
	

	checkkeypressed:  	;check if any key pressed
	
		mov ah,1
		int 16h
		jnz fill		;if key pressed jmp to fill data
		call readdata	;if not check if any data recieved
EndgameFlag:
		cmp gameEnd,1
		jz EndT3
		cmp readtest,1
		jnz contread
		jmp disp
	contread:
		
		jmp checkkeypressed	;loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	fill:				;fill the send data with the pressed key
	    mov sendvalue,ah	
		call senddata	;send the data
	   mov al,asciisendvalue
		mov ah,sendvalue
	  call jerryturn
      call  drawjerry 	;print the send data
	    call readdata  ;check if data is recieved
EndG:		cmp gameEnd,1
		jz EndgameFlag
		cmp readtest,1
		jnz contread2
		jmp disp
	contread2:
		
		mov ax,0		;clear the keyboard buffer
		int 16h
		jmp disp       ;loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

disptom:  
mov bh,0
mov readtest,bh            ;Start displaying tom and his functionality
    mov bx,xtom       ;check if tom changes his position then jump to clear and if not jump to do nothing
    cmp bx,xtomn
    jne cleartom2
	mov bx,ytom
	cmp bx,ytomn
	je contindonothingtom
	jmp cleartom2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
cleartom2:
	mov bl,tomtest
	cmp bl,0
	jz outofrange
	MOV BX,celltomold   ;check if tom previous cell is jerry then don't clear
	CMP BX,celljerry
	jz outofrange		 				
	mov cx,xtom         ;clear by drawing a cell after checking if it's a white cell or grey cell
	mov dx,ytom
	sub dx,13
	sub cx,13
	mov bx,offset gridarr
	add bx,celltomold
	push cx
	mov cl,9
	cmp [bx],cl
	jne conttrap
	pop cx
	MOV SI,OFFSET Greycolor
	JMP CONTCELL
	conttrap:
	pop cx
	MOV SI,OFFSET GridWhiteColor
	CONTCELL:
   	MOV BX,LEN
	call drawcell
	jmp checkcheestom
	EndgameFlagtemp:
	jmp EndG
    contindonothingtom:  ;Temporary jump
    jmp donothingtom
 ;;;;;;;;;;;;;;;                     
   checkcheestom:         ;check if tom previous cell is cheese then draw it again after clearing
	mov si,offset gridarr 
    add si,celltomold
    mov cl,[si]	  
	cmp cl,5
	jnz checkfreetom
	MOV CX,xtom
    MOV DX,ytom
    call  DRAWCHEESE 
	jmp checkfreetom
	outofrange:  
	jmp drwtom
	
	
	
	
	checkfreetom:  ;check if tom previous cell is Remove trap then draw it again after clearing
	mov si,offset gridarr  ;check freetom
    add si,celltomold
    mov cl,[si]	  
	cmp cl,7
	jnz comp
	MOV CX,xtom
    MOV DX,ytom
    call  DRAWheart
	jmp comp
	disptemp:
	jmp disptom
;;;;;;;;;;;;;;;;;;;
comp:   ;continue tom turn
    mov bl,0
	mov tomtest,bl
    drwtom: 
	call drawtom ;start drawing tom
	donothingtom:	
	call playtom	;update tom weapons based on his current position
EndTemp:
	cmp gameEnd,1
	jz EndgameFlagtemp
	CALL UPDATESTATUS  ;update the status bar based on what weapons tom takes
checkkeypressedtom:  	;check if any key pressed
	
		mov ah,1
		int 16h
		jnz filltom		;if key pressed jmp to fill data
		call readdatatom	;if not check if any data recieved
EndTemp2:
		cmp gameEnd,1
	    jz EndTemp
		cmp readtest,1
		jnz contreadtom
		jmp disptemp
	contreadtom:
		cmp endrequest,1 ;check if the Esc is pressed
		jz Exitprog
		jmp checkkeypressedtom	;loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	filltom:				;fill the send data with the pressed key
	    mov sendvalue,ah	
		call senddata	;send the data
	    cmp al,1bh		;if esc exit the prog
		je Exitprog
		mov ah,sendvalue
	  call tomturn
      call  drawtom 	;print the send data
		
	    call readdatatom  ;check if data is recieved
		cmp gameEnd,1
	    jz EndTemp2
		cmp readtest,1
		jnz contread2tom
		jmp disptemp
	contread2tom:
		cmp endrequest,1
		jz Exitprog
		mov ax,0		;clear the keyboard buffer
		int 16h
		jmp disptemp       ;loop
	Exitprog:	
		 MOV AH,4CH
		 INT 21H
				
MAIN  ENDP 
END MAIN