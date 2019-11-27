;*********************************************************************************
;                    PROYECTO_FINAL:  RADAR_623.
;*********************************************************************************
;           FECHA: 11 DICIEMBRE 2019
;           AUTOR: Pablo Vargas 
;           CARNE: B57564
;           CURSO: Microprocesadores.
;           PROFESOR: Geovanny Delgado.
;
; DESCRIPCION: *******   HACER UNA DESCRIPCION BUENA  ***********
;               
;              
;.................................................................................
;.................................................................................
;                   DECLARACION DE LAS ESTRUCTURAS DE DATOS
;.................................................................................

                ORG $1000

;------BANDERAS:
Banderas_H:       ds 1      ;BANDERAS_H =   X       :   X  :      X     : X      :X          : X       : CNF_FLG   : S1
Banderas_L:       ds 1      ;BANDERAS_L =  FREE_FLG : C_D  : CALC_TICKS : ALERTA : PANT_FLAG :ARRAY_OK : TCL_LEIDA : TCL_LISTA
;------VARIABLES:
V_LIM:          ds 1
MAX_TCL:        db 2
Tecla:          ds 1
Tecla_IN:       ds 1
Cont_Reb:       ds 1
Cont_TCL:       ds 1
Patron:         ds 1
Num_Array:      ds 2
BRILLO:         ds 1
POT:            ds 1
TICK_EN:        ds 2
TICK_DIS:       ds 2
VELOC:          ds 1
TICK_VEL:       ds 1
BIN1:           ds 1
BIN2:           ds 1
BCD1:           ds 1                   
BCD2:           ds 1              ; Valores en 7 segmentos para cada display
BCD_L:          ds 1
BCD_H:          ds 1
DISP1:          ds 1
DISP2:          ds 1
DISP3:          ds 1
DISP4:          ds 1
LEDS:           ds 1
CONT_DIG:       ds 1
CONT_TICKS:     ds 1
DT:             ds 1 
CONT_7SEG:      ds 2
CONT_200:       ds 1
Cont_Delay:     ds 1
D2mS:           db 100            ; Constantes de tiempo para la subrutina DELAY
D260uS:         db 13
D60uS:          db 3
Clear_LCD:      db $01
ADD_L1:         db $80            ; Posiciona la panatalla en la linea 1
ADD_L2:         db $C0            ; Posiciona la panatalla en la linea 2


Ajuste:         db 0              ; Constante usada en el programa del teclado

;------TABLAS:
                ORG $1030
Teclas:         db $01,$02,$03,$04,$05,$06,$07,$08,$09,$0B,$00,$0E

                ORG $1040
SEGMENT:        db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F,$40,$00

                ORG $1050
IniDsp:         db $04,$28,$28,$06,$0C
                db $FF

;------- MENSAJES:

                ORG $1060

CNF_MEN_L1:     FCC "MODO CONFIG      "
                db $FF
CNF_MEN_L2:     FCC "VELOC. LIMITE   "
                db $FF
MED_MEN_L1:     FCC "MODO MEDICION:   "
                db $FF
MED_MEN_L2:     FCC "SU VEL. VEL.LIM"
                db $FF
MED_MEN_L3:     FCC "ESPERANDO...    "
                db $FF
MED_MEN_L4:     FCC "CALCULANDO...    "
                db $FF
LIB_MEN_L1:     FCC "RADAR   623      "
                db $FF
LIB_MEN_L2:     FCC "MODO LIBRE        "
                db $FF






CUENTA:         ds 1
ACUMUL:         ds 1
CPROG:          ds 1
VMAX:           db 250     
TIMER_CUENTA:   ds 1
LOW:            ds 1
                           
;.................................................................................
;                      DECLARACION DEL VECTOR DE INTERRUPCIONES
;.................................................................................

                ORG $3E4C       ;PHO
                dw CALCULAR
;--------------------------------------------------------------------------------
                ORG $3E66       ;OC4
                dw OC4_ISR
;--------------------------------------------------------------------------------
                ORG $3E70       ;RTI
                dw RTI_ISR
;--------------------------------------------------------------------------------
                ORG $3E52       ;ATD
                dw ATD0_ISR
;--------------------------------------------------------------------------------
                ORG $3E5E       ;TCNT
                dw TCNT_ISR
;--------------------------------------------------------------------------------



;-----------------------Etiquetas con Registros-----------------------------
;---------------------------------------------------------------------------
#include registers.inc
;.................................................................................
;.................................................................................
;                             PROGRAMA PRINCIPAL
;.................................................................................

                ORG $2000
                lds #$3bff          ; Se define la pila 

;__________________________________________________________________________________
; CONFIGURACION DE HARWARE:
;__________________________________________________________________________________

;//////////////////////////////////////// RTC /////////////////////////////////////////

                movb #$23,RTICTL           ; Se configura un tiempo de 1ms para la RTI
                bset CRGINT,#$80           ; Se habilitan las interrupciones RTI

;////////////////////////////////////// Puerto A ///////////////////////////////////////

                movb #$F0,DDRA          ; Define las entradas y salidas del puerto A
                bset PUCR, $01               ; Pone las resistencias de pull up en el puerto A

;////////////////////////////////////// Puerto H ///////////////////////////////////////

                bset PIEH,$0F           ; Habilita la interrupcion PH0-PH3 y Mod_Sel

;/////////////////////////////////////// TCNT /////////////////////////////////////////////
                bset TSCR1,$90          ; Habilita el TC
                bset TSCR2,$83          ; Define el preescalador en 8

;/////////////////////////////////////// OC4 /////////////////////////////////////////////

                bset TIOS,$10          ; Habilita la salida 4
                bset TIE,$10

; //////////////////////////////////  LEDS y DISPLAYS ////////////////////////////////////
                movb #$FF, DDRB         
                bset DDRJ,$02         ;Habilita el pin 2 del puerto J
                bset PTJ,$02          
                movb #$0f, DDRP
                movb #$0f, PTP
                
; ////////////////////////////// PANTALLA LCD //////////////////////////////////////////

                bset DDRK,$FF

;///////////////////////////////////// ATD ////////////////////////////////////////////
                movb #$82,ATD0CTL2      ; Activa el ATD y las interrupciones
                ldaa #80
ATDLOOP:        dbne a,ATDLOOP          ;Espera 10us
                movb #$30,ATD0CTL3      ; Realiza 6 conversiones
                movb #$B7,ATD0CTL4      ; Define la conversion a 8-bits / 4 periodos ATD / frecuencia en 500KHz

;_________________________________________________________________________________
; INICIALIZACION DE VARIABLES:
;_________________________________________________________________________________


                movb #$FF,Tecla               
                movb #$FF,Tecla_IN        
                
                ldx  #Num_Array
                ldaa MAX_TCL                  ; Posiciona el puntero al final del array
                leax a,x
                
RESET_NUM_ARRAY:
                movb #$FF, 1,-x                ; Inicializa Num_Array con $FF
                cpx #Num_Array
                bne RESET_NUM_ARRAY 

; RESET DE VARIABLES:                
                clr Cont_Reb                  
                clr Cont_TCL
                clr Banderas_H
                clr banderas_L
                clr Patron
                clr LEDS
                clr V_LIM                     
                clr BCD1
                movb #$BB,BIN1
                clr BCD2
                movb #$BB,BIN2
                clr BRILLO
                clr POT
                movw #0, TICK_EN        
                movw #0,TICK_DIS       
                clr VELOC
                clr TICK_VEL       
                clr BCD_L
                clr DISP1    
                clr DISP2    
                clr DISP3    
                clr DISP4    
                clr CONT_DIG 
                clr CONT_TICKS
                movw #5000,CONT_7SEG
                clr DT       
                clr CONT_200 
                clr Cont_Delay
                CLI                                 ; Activa interrupciones
; INICIALIZA EL OC4:

                ldd TCNT
                addd #60
                std TC4

;---------------------------------------------------------------------------------
; Se realiza el ciclo de configuracion de la pantalla

                ldx #IniDsp
Config_Loop:    ldaa 1,x+          ; Comienza el ciclo de configuracion del LCD
                cmpa #$FF
                beq Clr_LCD
                bclr Banderas_L,$40    ; Se va a enviar un comando
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
                bra Config_Loop
Clr_LCD:        bclr Banderas_L,$40
                ldaa Clear_LCD               ; Comando para hacer clear
                jsr SEND
                movb D2mS,Cont_Delay
                jsr DELAY

; INICIO DEL PROGRAMA PRINCIPAL:


MAIN_LOOP:      bset Banderas_L,$10
                jsr MODO_LIBRE
                bra *

                brset PTIH,$40, MOD_MED
                clr PIEH           ; Inhabilita la interrupcion PH0 y PH3
                bclr TSCR2,$80          ; Inhabilita el TC
                brclr PTIH,$80, MOD_CNF
                bclr Banderas_H,$02
                bclr Banderas_H,$04
                jsr MODO_LIBRE
                bra MAIN_LOOP
                
MOD_MED:
                brset Banderas_H,$04,NO_MEN_MED
                bset Banderas_H,$04
                ldx #MED_MEN_L1
                ldy #MED_MEN_L3
                jsr CARGAR_LCD
NO_MEN_MED:     movb  #$02,LEDS
                bclr Banderas_L,$80    ;BORRA LAS BANDERAS CNF_FLG y FREE_FLG
                bclr Banderas_H,$02
                bset PIEH,$0F
                bset TSCR2,$80

                jsr MODO_MEDICION
                bra MAIN_LOOP

MOD_CNF:
                movb #$01,LEDS
                bclr Banderas_L,$80
                bclr Banderas_H,$04
                ;movb #$BB, BIN1     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;movb #$BB, BIN2     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;

                ;ldx #CNF_MEN_L1
                ;ldy #CNF_MEN_L2
                jsr MODO_CONFIG
                bra MAIN_LOOP
;                tst CPROG
;                beq SET_MODO_CONF
;                brset PTIH,$80,MODSEL_1 ; CONFIG: mod_sel = 1 / RUN: mod_sel = 0 DIPS_WITCH
;                bclr Banderas,$10          ; Pone mod_sel en 0: Modo RUN
;                bra Main_Sigue
;MODSEL_1:       bset Banderas,$10          ; Pone mod_sel en 1: Modo Config
;
;Main_Sigue:
;                brclr Banderas,$10,CAMBIO_MODO_RUN
;
;CAMBIO_MODO_CONF:
;                brclr Banderas,$20,Conf         ; Revisa si hubo un cambio de modo
;SET_MODO_CONF:  bclr Banderas,$20         ; Si hubo cambio de modo hace un toggle
;                bset Banderas,$10     ;Pone mod_sel en 1 al entrar a modo config
;                      movb #$02,LEDS
;                clr CUENTA             ; Reinicia las cuentas al ingresar a modo config
;                clr ACUMUL
;                clr CPROG
;                clr BIN1
;                bclr PORTE,$04
;                ldx #CNF_MEN_L1         ; Carga los mensajes del modo config
;                ldy #CNF_MEN_L2
;                jsr CARGAR_LCD
;Conf:           jsr MODO_CONFIG
;                bra CONVERTIR_BIN_BCD

;CAMBIO_MODO_RUN:
;                brset Banderas,$20,Run        ; Revisa si hubo un cambio de modo
;SET_MODO_RUN:   bset Banderas,$20             ; Si hubo cambio de modo hace un toggle
;                movb #$01,LEDS
;                ldx #RUN_MEN_L1               ; Carga los mensajes del modo run
;                ldy #RUN_MEN_L2
;                jsr CARGAR_LCD
;Run:
;                jsr MODO_RUN
;                ldaa CUENTA                   ; Si CPROG = CUENTA se enciende el RELAY
;                cmpa CPROG
;                beq ACTIVAR_RELAY
;                bclr PORTE,$04                ; Apaga el RELAY
;                bra CONVERTIR_BIN_BCD
;
;ACTIVAR_RELAY:  bset PORTE,$04                ; Enciende el RELAY

;CONVERTIR_BIN_BCD:
;                jsr CONV_BIN_BCD
;                lbra MAIN_LOOP


;.................................................................................
;                             SUBRUTINA MODO_CONFIG
;.................................................................................
; Esta subrutina se encarga de recibir la cantidad de tornillos a contar y verifica
;  que la cantidad ingresada sea un valor permitido entre 12 y 96.

MODO_CONFIG:
                brset Banderas_H,$02,INIT_MOD_CNF
                bset Banderas_H,$02
                ldx #CNF_MEN_L1
                ldy #CNF_MEN_L2
                jsr CARGAR_LCD
INIT_MOD_CNF:   brset Banderas_L,$04,Valor_Listo ;Verificando si Ya hay una tecla lista
                jsr TAREA_TECLADO
                ;movb V_LIM,BIN1
                rts
Valor_Listo:
                jsr BCD_BIN          ;Convierte de BCD a binario el valor
                bclr Banderas_L,$04   ; Pone en cero la bandera array_ok
                ldx #Num_Array
                movb #$FF,1,x+
                movb #$FF,0,x
                ldaa V_LIM
                cmpa #45
                blo Valor_INV       ; Se valida el valor
                cmpa #90
                bhi Valor_INV
                movb V_LIM,BIN1     ; Mueve el valor valido ingresado a BIN1
                movb $BB,BIN2
                rts
Valor_INV:
                clr V_LIM           ; Valor no valido
                rts


;.................................................................................
;                             SUBRUTINA MODO_LIBRE
;.................................................................................
; Esta subrutina se encarga de recibir la cantidad de tornillos a contar y verifica
;  que la cantidad ingresada sea un valor permitido entre 12 y 96.

MODO_LIBRE:     brset Banderas_L,$80,LIBRE_RETORNO
                bset Banderas_L, $80
                ldx #LIB_MEN_L1
                ldy #LIB_MEN_L2
                jsr CARGAR_LCD
                movb #$BB, BIN1
                movb #$BB, BIN2



LIBRE_RETORNO:
                movb #$04, LEDS
                rts


;.................................................................................
;                              SUBRUTINA MODO_MEDICION
;.................................................................................
;Descripcion: Esta subrutina se encarga de 
; 

MODO_MEDICION:
                tst VELOC
                beq MED_RETORNO
                jsr PANT_CTL
MED_RETORNO:    
                rts


;.................................................................................
;                              SUBRUTINA PANT_CTL
;.................................................................................
;Descripcion: Esta subrutina se encarga de 
; 
PANT_CTL:       
                clr PIEH            ; Apaga las interrupciones del puerto H (SENSORES)
                ldaa VELOC 
                cmpa #30
                blo FUERA_RANGO
                ldaa VELOC           ; SOLO PARA PROBAR
                cmpa #99
                bgt FUERA_RANGO
                ldaa VELOC           ; TAMBIEN __:_;:;;:_:;:_:;:;:_;:_:
                cmpa V_LIM
                bgt Alerta_1
                bra PANT_SIGUE

Alerta_1:       bset Banderas_L,$10
                bra PANT_SIGUE

FUERA_RANGO:    movb #1, TICK_EN
                movb #92, TICK_DIS      ; Programa 2 segundos
                movb #$AA, VELOC
                bra CHECK_PF

PANT_SIGUE:     brset Banderas_L,$20,CHECK_PF                
                ldaa VELOC
                tfr a,x
                ldd #16560
                idiv
                stx TICK_EN
                tfr x,d
                lsld
                ldd TICK_DIS
                bset Banderas_L,$20
                bra PANT_RETORNO
                
CHECK_PF:       brset Banderas_L,$08,MOSTRAR_VELOC
                ldaa BIN1 
                cmpa #$BB
                beq PANT_RETORNO
                ldx #MED_MEN_L1
                ldy #MED_MEN_L3
                jsr CARGAR_LCD
                movb #$BB, BIN1
                movb #$BB, BIN2
                clr VELOC
                movb #$0F, PIEH
                bra PANT_RETORNO

MOSTRAR_VELOC:  ldaa BIN1
                cmpa #$BB
                bne PANT_RETORNO
                ldx #MED_MEN_L1
                ldy #MED_MEN_L2
                jsr CARGAR_LCD
                movb V_LIM, BIN1
                movb VELOC, BIN2


PANT_RETORNO:   rts

;.................................................................................
;                             SUBRUTINA BCD_BIN
;.................................................................................
; Esta subrutina se encarga de realizar la conversion de los numeros ingresados
;  por teclado, los cuales estan en BCD a formato binario.

BCD_BIN:
                ldx #Num_Array
                ldaa 1,x+
                ldab #$0a
                mul
                ldaa 0,x
                cmpa #$FF
                bne Valor_Correcto
                clr V_LIM
                rts
Valor_Correcto: aba
                staa V_LIM
                rts
;.................................................................................
;                             SUBRUTINA BCD_7SEG
;.................................................................................
; Esta subrutina se encarga de realizar la conversion de los numeros en BCD al
; formato 7 segmentos.

BCD_7SEG:
                ldx #SEGMENT
                ldaa BCD1           
                anda #$0f            ; Parte baja de BIN1 a DISP4
                movb a,x,DISP4
                ldaa BCD1
                anda #$f0            ; Parte alta de BIN1 a DISP3
                lsra
                lsra
                lsra
                lsra
                movb a,x,DISP3

Seguir_BCD2:    ldab BCD2             
                andb #$0f            ; Parte baja de BIN2 a DISP2
                movb b,x,DISP2
                ldab BCD2
                andb #$f0            ; Parte alta de BIN2 a DISP1
                lsrb
                lsrb
                lsrb
                lsrb
                movb b,x,DISP1
                rts
                
;.................................................................................
;                             SUBRUTINA CONV_BIN_BCD
;.................................................................................
; Esta subrutina se encarga de realizar la conversion BIN_BCD de los numeros ingresados
;
CONV_BIN_BCD:   ldaa BIN1             ; Se llama al algoritmo para BIN1 y BIN2
                cmpa $BB
                beq BCD1_BB
                cmpa $AA 
                beq BCD1_AA
                jsr BIN_BCD
                movb BCD_L,BCD1
                bra CHECK_BIN2

BCD1_BB:        movb #$BB, BCD1         ; Si BIN1 = $BB -> BCD1 = $BB
                bra CHECK_BIN2

BCD1_AA:        movb #$AA, BCD1
                bra CHECK_BIN2          ; Si BIN1 = $AA -> BCD1 = $AA

CHECK_BIN2      ldaa BIN2
                cmpa $BB
                beq BCD2_BB
                cmpa $AA 
                beq BCD2_AA
                jsr BIN_BCD
                movb BCD_L,BCD2
                bra CONV_RETORNO
                
BCD2_BB:        movb #$BB, BCD2         ; Si BIN2 = $BB -> BCD2 = $BB
                bra CONV_RETORNO

BCD2_AA:        movb #$AA, BCD2
                bra CONV_RETORNO        ; Si BIN2 = $AA -> BCD2 = $AA

CONV_RETORNO:
                rts


;.................................................................................
;                             SUBRUTINA BIN_BCD
;.................................................................................
; Esta subrutina se encarga de realizar la conversion BIN_BCD de los numeros ingresados
;

BIN_BCD:        ldab #$07         ; Este es el algoritmo que realiza la conversion
                clr BCD_L                 
LOOP1:          lsla
                rol BCD_L
                psha
                ldaa #$0F
                anda BCD_L
                cmpa #5
                blo SIGA1
                adda #3
SIGA1:          staa LOW
                ldaa #$F0
                anda BCD_L
                cmpa #$50
                blo SIGA2
                adda #$30
SIGA2:          adda LOW
                staa BCD_L
                pula
                decb
                bne LOOP1
                lsla
                rol BCD_L
                rts

;.................................................................................
;                             SUBRUTINA PATRON_LEDS
;.................................................................................
; Esta subrutina genera un retardo programable por el usuario utilizando OC4

PATRON_LEDS: 
                brclr Banderas_L,$10,SIN_PATRON
                ldab #$F8
                andb LEDS
                lsrb
                cmpb #$08
                blo AJUSTA_LED
                ldaa #$07
                anda LEDS
                aba
                staa LEDS
                bra PATRON_RETORNO

SIN_PATRON:     ldaa #$07
                anda LEDS
                staa LEDS
                bra PATRON_RETORNO

AJUSTA_LED:     bset LEDS, $80
                bclr LEDS, $08
                bra PATRON_RETORNO

PATRON_RETORNO: rts


;.................................................................................
;                             SUBRUTINA DELAY
;.................................................................................
; Esta subrutina genera un retardo programable por el usuario utilizando OC4

DELAY:      
Delay_Loop:     tst Cont_Delay
                beq Retornar_Delay
                bra Delay_Loop
Retornar_Delay: rts

;.................................................................................
;                             SUBRUTINA CARGAR LCD
;.................................................................................
; Esta subrutina se encarga de poner los mensajes en las lineas 1 y 2 de la
;  pantalla LCD

CARGAR_LCD:
                bclr Banderas_L, $40   ; Para enviar un comando
                ldaa #$80             ; Se ubica en la primera linea del LCD
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
LoopM1          bset Banderas_L,$40    ; Para enviar un Dato
                ldaa 1,x+
                cmpa #$FF
                beq L2
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
                bra LoopM1
L2:             bclr Banderas_L, $40   ; Para enviar un comando
                ldaa #$C0              ; Se ubica en la Segunda linea del LCD
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
LoopM2:         bset Banderas_L,$40    ; Para enviar un Dato
                ldaa 1,y+
                cmpa #$FF
                beq RetornarLCD
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
                bra LoopM2
RetornarLCD:    rts


;.................................................................................
;                             SUBRUTINA SEND (COMMAND / DATA)
;.................................................................................
; Esta subrutina se encarga de enviar un commando o un dato a la pantalla LCD, 
;  y estos se envian a travez de PORTK.2-PORTk.5

SEND:
                psha
                anda #$F0
                lsra
                lsra
                staa PORTK
                brset Banderas_L,$40, Dato1
Comando:        bclr PORTK,$01
                bra Enable1
Dato1:          bset PORTK,$01
Enable1:        bset PORTK,$02
                movb D260uS,Cont_Delay
                jsr DELAY
                bclr PORTK,$02
                pula
                anda #$0F
                lsla
                lsla
                staa PORTK
                brset Banderas_L,$40, Dato2
                bclr PORTK,$01
                bra Enable2
Dato2:          bset PORTK,$01
Enable2:        bset PORTK,$02
                movb D260uS,Cont_Delay
                jsr DELAY
                bclr PORTK,$02
                rts

;.................................................................................
;                             SUBRUTINA TAREA_TECLADO
;.................................................................................
; Esta subrutina gestiona todo el proceso de ingreso y procesado de la tecla

Tarea_Teclado:  tst Cont_Reb
                bne Retorno
                jsr Mux_Teclado
                brset Tecla,$FF,ProcesaTecla
                brset Banderas_L,$02,Tecla_Leida    ; Revisa que la tecla este leida
                movb Tecla, Tecla_IN
                bset Banderas_L, $02                  ; Pone la tecla como leida
                movb #$0A,Cont_Reb                 ; Pone 10 al contador de rebotes
                bra Retorno
Tecla_Leida:    ldaa Tecla
                cmpa Tecla_IN
                beq Tecla_Lista                
                movb #$FF,Tecla
                movb #$FF,Tecla_IN
                bclr Banderas_L,$03       ; Borra las banderas TCL_LISTA y TCL_LEIDA
                bra Retorno
Tecla_Lista:    bset Banderas_L, $01      ; Pone la bandera TCL_LISTA en 1
                bra Retorno

ProcesaTecla:   brset Banderas_L,$01,Guardar_Tecla  ; Si la tecla esta lista la guarda
                bra Retorno

Guardar_Tecla:  bclr Banderas_L,$03       ; Borra las banderas TCL_LISTA y TCL_LEIDA
                jsr Formar_Array
Retorno:        rts

;.................................................................................
;                         SUBRUTINA MUX_TECLADO
;.................................................................................
; Esta subrutina revisa constantemente el teclado para ver si una tecla fue presionada

Mux_Teclado:    movb #$FF, Tecla
                ldaa #$EF           ; Primer patron
                movb #$01,Patron
Loop_Teclado:   ldab Patron
                cmpb #$04
                bgt Retorno_Teclado
                staa PORTA           ; Envia el patron al puerto A
                movb #$03,Ajuste
                brclr PORTA,$01,Calculo
                dec Ajuste                  ; Busca en las columnas si hay alguna tecla presionada
                brclr PORTA,$02,Calculo
                dec Ajuste
                brclr PORTA,$04,Calculo
                inc Patron
                asla                    ; Cambia al siguiente patron
                bra Loop_Teclado
Calculo:        ldaa Patron
                ldab #$03               ; Usando el patron y el ajuste calculado con el barrido de las columnas
                mul                     ; Calcula el indice de la tecla para buscarlo en la tabla de teclas
                tfr b,a
                ldab Ajuste
                sba
                clrb
                ldy #Teclas
                movb a,y,tecla
Retorno_Teclado:rts

;.................................................................................
;                         SUBRUTINA FORMAR_ARRAY
;.................................................................................
; Esta subritina se encarga de guardar en un arreglo las teclas ingresadas por el usuario
; e ignorar las teclas que no correspondan o en el momento en que no correspondan
Formar_Array:
                ldy #Num_Array
                ldab Cont_TCL
                cmpb MAX_TCL
                beq Ultima_Tecla   ; Si es la ultima tecla solo puede recibir borrar o enter
                ldaa Tecla_IN
                cmpa #$0B
                beq Borrar_T1
                ldaa Tecla_IN
                cmpa #$0E
                beq Enter_T1 
                movb Tecla_IN,b,y
                inc Cont_TCL
                bra Retorno_Array

Borrar_T1:      tst Cont_TCL         ; Prueba si es la primera tecla que se ingresa para ignorar el enter y el borrar
                bne Borrar
                bra Retorno_Array
Enter_T1:       tst Cont_TCL
                bne Enter
                bra Retorno_Array

Ultima_Tecla:   ldaa Tecla_IN
                cmpa #$0B
                beq Borrar
                ldaa Tecla_IN
                cmpa #$0E
                beq Enter
                bra Retorno_Array

Enter:          bset Banderas_L,$04    ; Cuano se da enter Array_OK pasa a 1
                clr Cont_TCL            
                bra Retorno_Array

Borrar:         dec Cont_TCL 
                ldab Cont_TCL
                movb #$FF,b,y         ; Llena los espacios borrados con FF
Retorno_Array:  rts









;.................................................................................
;                        SUBRUTINA DE ATENCION DE INTERUPCION PH0
;.................................................................................

CALCULAR:       bclr Banderas_L,$04      ; Pone Array_OK en cero
                brset PIFH,$08,PTH3
                brset PIFH,$01,PTH0
                rti

PTH0:           brclr Banderas_H,$01,CALC_RETORNO
                ldab TICK_VEL
                tfr b,x
                ldd #6624
                idiv
                tfr x,a
                staa VELOC
                clr TICK_VEL
                bclr Banderas_L,$20
                bset PIFH,$01
                rti
                bra CALC_RETORNO
                

PTH3:           
                clr TICK_VEL
                bset Banderas_H,$01  ;Se asegura el sentido correcto
                ldx #MED_MEN_L1
                ldy #MED_MEN_L4
                jsr CARGAR_LCD
                bset PIFH,$08
                rti
		bra CALC_RETORNO

CALC_RETORNO:   ;bset PIFH,$04       ; bajando la bandera
                rti

;.................................................................................
;         SUBRUTINA DE ATENCION DE INTERRUPCIONES OUTPUT COMPARE CHANNEL 4
;.................................................................................
; Esta subrutina define un contador con uso de la interrupcion OC4.
; Decrementa Cont_Delay y carga TC4

OC4_ISR:
                tst CONT_7SEG
                bne Sigue
                movw #5000,CONT_7SEG

                jsr CONV_BIN_BCD
                jsr BCD_7SEG
Sigue:          ldaa CONT_TICKS
                cmpa #100                       ; Revisa si pasaron 100 TICKS
                blt Puente1
                movb #$00, CONT_TICKS
                inc CONT_DIG            ; si CON_TICKS = 0 pone el numero el DISP correspondiente
                ldaa CONT_DIG
                cmpa #5
                blt Puente1
                clr CONT_DIG
                bset PTJ, $02                   ; Desconecta los leds
Puente1:        ldab #$F7
                tst CONT_TICKS
                beq DISPLAYS
                ldaa #100
                suba BRILLO
                staa DT
                ldaa CONT_TICKS
                cmpa DT
                blt Puente2
                movb #$FF,PTP

Puente2:        tst CONT_200
                bne Puente3
                movw #10000,CONT_200
                ;movb #$87,ATD0CTL5
                jsr PATRON_LEDS

Puente3:        tst Cont_Delay
                beq CargarTC4             ; Carga el OC4
                dec Cont_Delay
CargarTC4:      ldd TCNT
                addd #60                
                std TC4
                inc CONT_TICKS
                dec CONT_7SEG
                dec CONT_200
                rti

DISPLAYS:       ldaa CONT_DIG
                cmpa #0
                bne DIS3                   ; Hace el ciclo de refrescamiento
                movb DISP4,PORTB
                stab PTP
                lbra Puente2

DIS3:           rorb
                cmpa #1
                bne DIS2
                movb DISP3,PORTB
                stab PTP
                lbra Puente2
                
DIS2:           rorb
                cmpa #2
                bne DIS1
                movb DISP2,PORTB
                stab PTP
                lbra Puente2

DIS1:           rorb
                cmpa #3
                bne ENCENDER_LEDS
                movb DISP1,PORTB
                stab PTP
                lbra Puente2

ENCENDER_LEDS:  movb #$FF,PTP
                bclr PTJ,$02
                movb LEDS,PORTB
                lbra Puente2

;.................................................................................
;                        SUBRUTINA DE ATENCION DE INTERUPCION RTI
;.................................................................................
;
;
RTI_ISR:        bset CRGFLG,#$80    ; Se borra la bandera de la interrupcion RTI
                tst Cont_Reb
                beq Cuenta_Timer        ; Esta suburutina se dedica exclusivamente a reducir el contador de rebotes y
                dec Cont_Reb
Cuenta_Timer:   tst TIMER_CUENTA
                beq Retornar
                dec TIMER_CUENTA
Retornar:       rti

;.................................................................................
;                        SUBRUTINA DE ATENCION DE INTERUPCION TCNT
;.................................................................................
;
;
;
TCNT_ISR:
                inc TICK_VEL
                tst TICK_EN
                beq PF_1
                dec TICK_EN
                bra Continua
PF_1:           bset Banderas_L,$08
Continua:       tst TICK_DIS
                beq PF_0
                dec TICK_DIS
                bra TCNT_RETORNO
PF_0:           bclr Banderas_L,$08
                ldd TCNT
TCNT_RETORNO:   rti



;.................................................................................
;         SUBRUTINA DE ATENCION DE INTERRUPCIONES OUTPUT COMPARE CHANNEL 5
;.................................................................................
; Esta subrutina atiende la interrupcion del ATD realizando un promedio de las
;  6 mediciones ejecutadas y guardandolo en la variable Nivel_PROM

ATD0_ISR:
                ldd ADR00H
                addd ADR01H
                addd ADR02H
                addd ADR03H
                addd ADR04H
                addd ADR05H
                ldx #6
                idiv
                tfr x,a
                staa POT
                ldab #20
                mul
                ldx #255
                idiv
                tfr x,a
                staa BRILLO
                ;movb #$87,ATD0CTL5
                rti