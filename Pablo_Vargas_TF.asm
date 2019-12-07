;*********************************************************************************
;                    PROYECTO_FINAL:  RADAR_623.
;*********************************************************************************
;           FECHA: 11 DICIEMBRE 2019
;           AUTOR: Pablo Vargas 
;           CARNE: B57564
;           CURSO: Microprocesadores.
;           PROFESOR: Geovanny Delgado.
;
; DESCRIPCION: Este proyecto consiste en un sistema de medicion de velocidad 
;              vehicular llamado RADAR_623, que tiene como objetivo determinar 
;              si el conductor se encuentra dentro de los limites de velocidad
;              e informarle por medio de una pantalla su velocidad. 
;
;              Este proyecto hace uso de 
;
;
;.................................................................................
;.................................................................................
;                   DECLARACION DE LAS ESTRUCTURAS DE DATOS
;.................................................................................

                ORG $1000

;------BANDERAS:
Banderas:       ds 2   ;BANDERAS = X:X:X:X:MED_FLG:CNF_INI:CNF_FLG:S1:FREE_FLG:C_D:CALC_TICKS:ALERTA:PANT_FLAG:ARRAY_OK:TCL_LEIDA:TCL_LISTA 
;------VARIABLES:
V_LIM:          ds 1
MAX_TCL:        ds 2
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
CONT_200:       ds 2
Cont_Delay:     ds 1
D2mS:           db 100            ; Constantes de tiempo para la subrutina DELAY
D260uS:         db 13
D60uS:          db 3
Clear_LCD:      db $01
ADD_L1:         db $80            ; Posiciona la pantalla en la linea 1
ADD_L2:         db $C0            ; Posiciona la pantalla en la linea 2

LOW:            ds 1

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
                bset CRGINT,$80           ; Se habilitan las interrupciones RTI

;////////////////////////////////////// Puerto A ///////////////////////////////////////

                movb #$F0,DDRA          ; Define las entradas y salidas del puerto A
                bset PUCR, $01               ; Pone las resistencias de pull up en el puerto A

;////////////////////////////////////// Puerto H ///////////////////////////////////////

                bset PIEH,$0F           ; Habilita la interrupcion PH0 y PH3 
                bset PPSH,$09

;/////////////////////////////////////// TCNT /////////////////////////////////////////////
                bset TSCR1,$80          ; Habilita el TC
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
                movb #$A5,ATD0CTL4      ; Define la conversion a 8-bits / 4 periodos ATD / frecuencia en 500KHz

;_________________________________________________________________________________
; INICIALIZACION DE VARIABLES:
;_________________________________________________________________________________

                movb #2,MAX_TCL
                movb #$FF,Tecla               
                movb #$FF,Tecla_IN        
                
                ldx  #Num_Array
                ldaa MAX_TCL                  ; Posiciona el puntero al final del array
                leax a,x
                
RESET_NUM_ARRAY:
                movb #$FF, 1,-x                ; Inicializa Num_Array con $FF
                cpx #Num_Array
                bne RESET_NUM_ARRAY 
;_________________________________________
;||||||||| RESET DE VARIABLES  ||||||||||||  

                clr Cont_Reb                  
                clr Cont_TCL
                clr Banderas
                clr Banderas+1
                clr Patron
                clr LEDS
                clr V_LIM                     
                clr BCD1
                movb #$BB,BIN1
                clr BCD2
                movb #$BB,BIN2
                clr BRILLO
                clr POT
                movw #0,TICK_EN        
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
                movw #10000,CONT_200 
                clr Cont_Delay 
                CLI                                 ; Activa interrupciones

;__________________________________________
;|||||||||| INICIALIZA EL OC4:  |||||||||||

                ldd TCNT
                addd #60
                std TC4

;---------------------------------------------------------------------------------
; Se realiza el ciclo de configuracion de la pantalla

                ldx #IniDsp
Config_Loop:    ldaa 1,x+          ; Comienza el ciclo de configuracion del LCD
                cmpa #$FF
                beq Clr_LCD
                bclr Banderas+1,$40    ; Se va a enviar un comando
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
                bra Config_Loop
Clr_LCD:        bclr Banderas+1,$40
                ldaa Clear_LCD               ; Comando para hacer clear
                jsr SEND
                movb D2mS,Cont_Delay
                jsr DELAY

;................................................................................
;                     INICIO DEL PROGRAMA PRINCIPAL:
;................................................................................

MAIN_LOOP:
                brclr Banderas,$04,INIT_CNF   ; Inicia en modo configuracion
                brset PTIH,$40, MOD_MED
                clr PIEH                        ; Inhabilita las interrupciones de PH
                bclr TSCR2,$80                  ; Inhabilita el TC
                brclr PTIH,$80,MOD_CNF
                jsr MODO_LIBRE
                bra MAIN_LOOP

INIT_CNF:       clr PIEH
                bclr TSCR2,$80
                jsr MODO_CONFIG
                bra MAIN_LOOP
                
MOD_MED:
                bset PIEH,$09                  ; Activa las interrupciones de PH
                bset TSCR2,$80                 ; Activa el TC
                jsr MODO_MEDICION
                bra MAIN_LOOP


MOD_CNF:        jsr MODO_CONFIG
                bra MAIN_LOOP



;____________________________________________________________________________________________________
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\ SUBRUTINAS DE ATENCION A INTERRUPCIONES: /////////////////////////////
;____________________________________________________________________________________________________


;.....................................................................................
;                        SUBRUTINA DE ATENCION DE INTERUPCION PH
;.....................................................................................
; Esta subrutina de atencion a la interrupcion del puerto H se encarga de simular
; los sensores S1 y S2 y a partir de estas señales calcula la velocidad del vehiculo
; utilizando la siguiente formula:     _______________________
;                                     | VELOC_=_6624/TICK_VEL |
;
; PH3 --> Simula el sensor S1.
; PH0 --> Simula el sensor S2.
;...................................................................................... 

CALCULAR:
                
                brset PIFH,$08,PTH3
                brset PIFH,$01,PTH0
                rti


PTH0:           
                bset PIFH,$01                ; Quita la bandera de PH0
                tst Cont_Reb
                bne PH0_RETORNO
                movb #22,Cont_Reb
                brclr Banderas,$01,CALC_RETORNO
                bclr Banderas,$01                 ;BANDERA = S1
                bclr Banderas+1,$20               ;BANDERA = CALC_TICKS
                
                ldab TICK_VEL
                ldaa #00
                tfr d,x
                ldd #6624
                idiv
                tfr x,d
                cpd #$00FF
                bgt AJUSTE_8BITS
                stab VELOC
                bra PH0_Retorno
AJUSTE_8BITS:   movb #255,VELOC

PH0_RETORNO:    
                rti

PTH3:           
                bset PIFH,$08                 ; Quita la bandera de PH3
                tst Cont_Reb
                bne CALC_RETORNO
                movb #22,Cont_Reb
                clr TICK_VEL
                bset Banderas,$01  ;Se asegura el sentido correcto   BANDERA = S1
                
                ldx #MED_MEN_L1
                ldy #MED_MEN_L4
                cli
                jsr CARGAR_LCD
                
CALC_RETORNO:   
                rti
                

;.................................................................................
;         SUBRUTINA DE ATENCION DE INTERRUPCIONES OUTPUT COMPARE CHANNEL 4
;.................................................................................
; Esta subrutina se encarga de realizar una serie de tareas, las cuales se listan 
; a continuacion: 
;                   * LLAMA A LAS SUBRUTINAS:                     
;                       - CONV_BIN_BCD
;                       - BCD_7SEG
;                       - PATRON_LEDS
;                   * INICIALIZA UN CICLO DE ATD
;
;                   * MANEJA LOS CONTADORES   
;                       - Cont_Delay
;                       - CONT_200
;                       - CONT_7SEG
;                       - CONT_TICKS
;                       - CONT_DIG
;                   * REALIZA EL CICLO DE REFRESCAMIENTO DE LOS DISPLAYS Y LEDS    
;..................................................................................

OC4_ISR:
                tst Cont_Delay
                beq check_7seg             ; decrementa cont_delay
                dec Cont_Delay

check_7seg:     ldx CONT_7SEG
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
                bset PTJ,$02                   ; Desconecta los leds

Puente1:        ldab #$F7
                tst CONT_TICKS
                beq DISPLAYS
                ldaa BRILLO
                ldab #5
                mul
                ldaa #100                  ;DT= 100- 5*BRILLO
                sba
                staa DT
                ldaa CONT_TICKS
                cmpa DT
                blt Puente2
                movb #$FF,PTP

Puente2:
                ldx CONT_200
                bne CargarTC4
                movw #10000,CONT_200
                jsr PATRON_LEDS
                cli
                movb #$87,ATD0CTL5

CargarTC4:      
                inc CONT_TICKS             ; Incrementaq y decrementa los contadores
                ldx CONT_7SEG
                dex
                stx CONT_7SEG
                ldx CONT_200
                dex
                stx CONT_200
                ldd TCNT
                addd #60                
                std TC4
                bset TFLG1,$10             ; Borra la bandera
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
; Esta suburutina se dedica exclusivamente a reducir el contador de rebotes.
; No recibe ningun parametro en la entrada, pero su salida es Cont_Reb.
;.................................................................................

RTI_ISR:        bset CRGFLG,$80    ; Se borra la bandera de la interrupcion RTI
                tst Cont_Reb
                beq Retornar        
                dec Cont_Reb

Retornar:       rti

;.................................................................................
;                        SUBRUTINA DE ATENCION DE INTERUPCION TCNT
;.................................................................................
; Esta subrutina se encarga de realizar un conteo del tiempo que transcurre desde
; que el vehiculo pasa por el sensor S1 hasta que cruza el sensor S2, por medio de
; la variable TICK_VEL.
;
;  Tambien determina cuando se cambia el mensaje en la pantalla del MODO_MEDICION 
; y despliega la informacion de la velocidad en los DISPLAYS, por medio de la 
; bandera PANT_FLAG.
;.................................................................................

TCNT_ISR:
                inc TICK_VEL
                ldx TICK_EN
                cpx #0
                beq PF_1
                dex 
                stx TICK_EN
                bra Continua
PF_1:           bset Banderas+1,$08   ; BANDERA = PANT_FLAG

Continua:       ldx TICK_DIS
                cpx #0
                beq PF_0
                dex
                stx TICK_DIS
                bra TCNT_RETORNO

PF_0:           bclr Banderas+1,$08   ; BANDERA = PANT_FLAG


TCNT_RETORNO:   bset TFLG2,$80
                rti

;....................................................................................
;         SUBRUTINA DE ATENCION DE INTERRUPCIONES ATD
;....................................................................................
; Esta subrutina atiende la interrupcion del ATD.
; Mide la conversion Analogico_Digital proveniente de uno de los potenciometros 
; de la Dragon 12.
; Toma como entrada los primeros 6 registros de resultados del convertidor ATD0      
; y realiza un promedio de las 6 mediciones hechas y el resultado del promedio
; lo guarda en la variable POT.
;.....................................................................................

ATD0_ISR:       bset ATD0STAT0,$80
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




;____________________________________________________________________________________________________
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ SUBRUTINAS DEL PROGRAMA: /////////////////////////////////////////
;____________________________________________________________________________________________________



;......................................................................................
;                             SUBRUTINA MODO_CONFIG
;......................................................................................
; Esta subrutina se encarga de configurar la velocidad limite permitida por el sistema
; y verifica que la cantidad ingresada sea un valor permitido entre 45 Km/H y 96 Km/H.
; Al entrar en este modo se borran las banderas FREE_FLG y MED_FLG para que se
; impriman los mensajes correspondientes al ingresar a los otros modos.
;  
;  Esta subritina hace llamado de las subrutinas:
;                                                  - CARGAR_LCD
;                                                  - BCD_BIN
;                                                  - TAREA_TECLADO
;
;  Tiene como salida la velocidad limite programada por medio de la variable V_LIM.
;.......................................................................................        

MODO_CONFIG:
                brset Banderas,$02,INIT_MOD_CNF   ; BANDERA = CNF_FLG
                bset Banderas,$02                 
                bclr Banderas+1,$80               ; BANDERA = FREE_FLG
                bclr Banderas,$08                 ; BANDERA = MED_FLG
                bclr Banderas+1,$10               ; BANDERA = ALERTA 
                movb #$BB,BIN2
                movb #$01,LEDS
                ldx #CNF_MEN_L1                   ; Carga el mensaje del modo configuracion      
                ldy #CNF_MEN_L2
                jsr CARGAR_LCD

INIT_MOD_CNF:   brset Banderas+1,$04,Valor_Listo     ; BANDERA = ARRAY_OK
                jsr TAREA_TECLADO
                movb V_LIM,BIN1
                bra CNF_FIN

Valor_Listo:
                jsr BCD_BIN                             ;Convierte de BCD a binario el valor
                bclr Banderas+1,$04                     ; Pone en cero la bandera array_ok
                ldx #Num_Array
                movb #$FF,1,x+
                movb #$FF,0,x
                ldaa V_LIM
                cmpa #45
                blo Valor_INV                           ; Se valida el valor
                cmpa #90
                bhi Valor_INV
                movb V_LIM,BIN1                         ; Mueve el valor valido ingresado a BIN1
                bset Banderas,$04                       
                bra CNF_FIN

Valor_INV:
                clr V_LIM           ; Valor no valido

CNF_FIN:
                bset PIFH,$09      ; Borra banderas en caso de que se presionen botones PH en este modo.
                rts
;...................................................................................
;                             SUBRUTINA MODO_LIBRE
;...................................................................................
;  Esta subrutina es el modo oscioso del sistema, en esta subrutina no se realiza
; ninguna operacion, solamente se imprime el mensaje del MODO_LIBRE y apaga los
; DISPLAYS.
;  Borra las banderas CNF_FLG, MED_FLG, ALERTA para reestablecer los otros modos.
;...................................................................................

MODO_LIBRE:     brset Banderas+1,$80,LIBRE_RETORNO   

                movb #$BB,BIN1             ; Apaga displays
                movb #$BB,BIN2
                movb #$04,LEDS                  
                clr VELOC                
                bclr Banderas,$02          ; BANDERA = CNF_FLG     
                bclr Banderas,$08          ; BANDERA = MED_FLG    
                bset Banderas+1,$80        ; BANDERA = FREE_FLG
                bclr Banderas+1,$10        ; BANDERA = ALERTA
                ldx #LIB_MEN_L1
                ldy #LIB_MEN_L2
                jsr CARGAR_LCD

LIBRE_RETORNO:  bset PIFH,$09
                rts

                
;...................................................................................
;                              SUBRUTINA MODO_MEDICION
;...................................................................................
; Esta subrutina se encarga de imprimir el mensaje correspondiente al 
; MODO_CONFIGURACION y dependiendo de si la velocidad registrada (VELOC) es mayor  
; que cero, llama a la subrutina PANT_CTL. 
;
; Tambien borra las banderas FREE_FLG y CNF_FLG para reestablecer los otros modos.
;...................................................................................

MODO_MEDICION:
                tst VELOC
                beq MENSAJE_MED

                jsr PANT_CTL
                bra MED_RETORNO

MENSAJE_MED:    brset Banderas,$08,MED_RETORNO        ; Si el mensaje ya se encuentra en pantalla, no lo pone de nuevo

                movb #$02,LEDS
                bset Banderas,$08            ; BANDERAS = MED_FLG
                bclr Banderas+1,$80          ; BANDERAS = FREE_FLG
                bclr Banderas,$02            ; BANDERAS = CNF_FLG 
                movb #$BB,BIN1
                ldx #MED_MEN_L1
                ldy #MED_MEN_L3
                jsr CARGAR_LCD            
MED_RETORNO:    rts
                


;.....................................................................................
;                              SUBRUTINA PANT_CTL
;.....................................................................................
;  Esta subrutina recibe como parametro la velocidad a travez de la variable VELOC
; y se asegura que se encuentre dentro del rango de los 30 Km/H a los 90 Km/H, 
; de no ser asi pone en los DISPLAYS de la izquierda dos guiones.
;  En caso de que este dentro del rango, revisa que se encuentra por debajo 
; del limite V_LIM. Si la velocidad supera el limite establecido por V_LIM 
; se activa el patron de leds y 2 segundos despues presenta la velocidad del 
; vehiculo en la pantalla. 
;
;  Esta subrutina tambien se encarga de calcular el tiempo necesario para que
; el conductor este a 100 m de la pantalla basado en la velocidad del conductor
; con el proposito de desplegar la informacion de la velocidad en el panel.
;  Para ello se utilizo la formula:
;                       _____________________________
;                      |    TICK_EN = 16560/VELOC    |
;
;  Tambien calcula el tiempo que le toma pasar el panel, el cual es el doble      
; del tiempo que le toma llegar a los 100 metros. Se calculo multiplicando
; TICK_EN por 2:
;                       _____________________________
;                      |    TICK_DIS = TICK_EN * 2   |
;
; Esta subrutina llama a la subrutina:
;
;                               - CARGAR_LCD
;                               
; Modifica la bandera ALERTA y toma decisiones con base en las banderas CALC_TICKS y
; PANT_CTL.                              
;......................................................................................

PANT_CTL:       
                clr PIEH            ; Apaga las interrupciones del puerto H (SENSORES)
                ldaa VELOC 
                cmpa #30
                blo FUERA_RANGO
                cmpa #99
                bhi FUERA_RANGO
                cmpa V_LIM
                bhi Alerta_1
                bra PANT_SIGUE

Alerta_1:       bset Banderas+1,$10     ; BANDERA = ALERTA
                bra PANT_SIGUE

FUERA_RANGO:    ldaa VELOC
                cmpa #$AA
                beq CHECK_PF
                movw #1,TICK_EN
                movw #92,TICK_DIS      ; Programa 2 segundos
                movb #$AA,VELOC
                movb #$BB,BIN1
                bra CHECK_PF

PANT_SIGUE:     brset Banderas+1,$20,CHECK_PF          ; BANDERA = CALC_TICKS      
                ldab VELOC
                ldaa #00
                tfr d,x
                ldd #16560
                idiv
                stx TICK_EN
                tfr x,d
                lsld
                std TICK_DIS
                bset Banderas+1,$20                     
                movb #$BB,BIN1
                bra PANT_RETORNO
                
CHECK_PF:       brset Banderas+1,$08,MOSTRAR_VELOC        ; BANDERA = PANT_FLAG
                ldaa BIN1 
                cmpa #$BB
                beq PANT_RETORNO
                
                ldx #MED_MEN_L1
                ldy #MED_MEN_L3
                movb #$BB,BIN1                            ; Asegura que los display esten apagados
                movb #$BB,BIN2
                clr VELOC
                movb #$0F, PIEH
                bclr Banderas+1,$10                        ; Apaga la alarma
                jsr CARGAR_LCD
                bra PANT_RETORNO

MOSTRAR_VELOC:  ldaa BIN1
                cmpa #$BB
                bne PANT_RETORNO
                ldx #MED_MEN_L1
                ldy #MED_MEN_L2
                movb V_LIM, BIN1
                movb VELOC, BIN2
                jsr CARGAR_LCD


PANT_RETORNO:   rts

;.................................................................................
;                             SUBRUTINA BCD_BIN
;.................................................................................
;  Esta subrutina se encarga de realizar la conversion de los numeros ingresados
; por teclado, los cuales estan en BCD y se pasan a formato binario.
;  Toma como entrada el vector de numeros que se encuentra en Num_Array y 
; guarda el resultado de la conversion en la variable V_LIM.
;.................................................................................

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
; formato 7 segmentos. Para ello recibe como parametros los valor de los display
; en formato BCD por medio de las variables BCD1 y BCD2 y de acuerdo con los 
; valores de la tabla SEGMENT asigna el codigo, en formato 7 segmentos, del
; digito correspondiente a cada display respectivamente. 
; 
; Esta subrutina tiene como salida a las variables DISP1 hasta DISP4.
;.................................................................................

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
; Esta subrutina se encarga de realizar la conversion de formato binario
; a formato BCD de los numeros ingresados.
;
; Esta subrutina recibe como parametros BIN1 y BIN2 los cuales contienen los
; numeros binarios a ser convertidos.
;
; Esta subrutina hace dos veces un llamado a la subrutina BIN_BCD, una para 
; convertir la variable BIN1 y la otra para la variable BIN2 y almacena los
; resultados en BCD1 y BCD2 respectivamente.
;
;.................................................................................

CONV_BIN_BCD:   ldaa BIN1             ; Se llama al algoritmo para BIN1 y BIN2
                cmpa #$BB
                beq BCD1_BB
                cmpa #$AA
                beq BCD1_AA
                jsr BIN_BCD
                movb BCD_L,BCD1
                bra CHECK_BIN2

BCD1_BB:        movb #$BB, BCD1         ; Si BIN1 = $BB -> BCD1 = $BB
                bra CHECK_BIN2

BCD1_AA:        movb #$AA, BCD1
                bra CHECK_BIN2          ; Si BIN1 = $AA -> BCD1 = $AA

CHECK_BIN2      ldaa BIN2
                cmpa #$BB
                beq BCD2_BB
                cmpa #$AA
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


;.........................................................................................
;                             SUBRUTINA BIN_BCD
;.........................................................................................
; Esta subrutina se encarga de realizar la conversion de formato binario a 
; formato BCD.
;
; Recibe los parametros por medio del acumulador A y es llamada 2 veces en la subrutina
; CONV_BIN_BCD pasando como parametros BIN1 y BIN2.
;
; La salida la hace por medio de la variable BCD_L.
;.........................................................................................

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
;  Esta subrutina se encarga de generar un patron de movimiento en los leds 
; empezando en PB7 y desplazandose hasta PB3 de forma repetida.
;  Este patron se utiliza para indicar que la velocidad del vehiculo es superior
; al limite de velocidad.
;
;  La salida de esta subrutina es la variable LEDS, la cual contiene cuales leds
; deben encenderse.
;
;.................................................................................

PATRON_LEDS: 
                brclr Banderas+1,$10,SIN_PATRON        ; BANDERA = ALERTA
                ldab #$F8
                andb LEDS
                lsrb
                cmpb #$08
                blt AJUSTA_LED
                ldaa #$07
                anda LEDS
                aba
                staa LEDS
                bra PATRON_RETORNO

SIN_PATRON:     ldaa #$07
                anda LEDS
                staa LEDS
                bra PATRON_RETORNO

AJUSTA_LED:     bset LEDS,$80
                bclr LEDS,$08
                bra PATRON_RETORNO

PATRON_RETORNO: rts


;.................................................................................
;                             SUBRUTINA DELAY
;.................................................................................
; Esta subrutina genera un retardo programable por el usuario utilizando OC4.
; Recibe como parametro Cont_Delay y espera hasta que sea cero para salir de 
; la subrutina.
;
;.................................................................................

DELAY:      
Delay_Loop:     tst Cont_Delay
                beq Retornar_Delay
                bra Delay_Loop
Retornar_Delay: rts

;.................................................................................
;                             SUBRUTINA CARGAR LCD
;.................................................................................
;  Esta subrutina se encarga de poner los mensajes en las lineas 1 y 2 de la
; pantalla LCD.
;  
;  Recibe como parametros los mensajes a desplegar en pantalla por medio de los 
; registros X y Y respectivamente para las lineas 1 y 2.
;
;  Esta subrutina llama a a su vez a las subrutinas:
;                                                       - DELAY      
;                                                       - SEND
;.................................................................................        

CARGAR_LCD:
                bclr Banderas+1,$40   ; Para enviar un comando
                ldaa #$80             ; Se ubica en la primera linea del LCD
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
LoopM1          bset Banderas+1,$40    ; Para enviar un Dato
                ldaa 1,x+
                cmpa #$FF
                beq L2
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
                bra LoopM1
L2:             bclr Banderas+1, $40   ; Para enviar un comando
                ldaa #$C0              ; Se ubica en la Segunda linea del LCD
                jsr SEND
                movb D60uS,Cont_Delay
                jsr DELAY
LoopM2:         bset Banderas+1,$40    ; Para enviar un Dato
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
;  y estos se envian a travez de PORTK.2-PORTk.5.
;
;  La decision de si se envia un Comando o un Dato se toma basado en el valor de
; la bandera C_D, donde un valor de cero significa que se envia un Comando, 
; mientras que un valor de 1 significa enviar un Dato.
;
;  La salida de esta subrutina es directamente hacia el puerto K, con el fin de
; interactuar con la pantalla LCD.
;.................................................................................

SEND:
                psha
                anda #$F0
                lsra
                lsra
                staa PORTK
                brset Banderas+1,$40, Dato1   ; BANDERA = C_D
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
                brset Banderas+1,$40,Dato2
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
; por medio de las variables Tecla y Tecla_IN.
; Tambien la subrutina hace llamado de las subrutinas:
;                                                  - MUX_TECLADO        
;                                                  - FORMAR_ARRAY
;
; Modifica las banderas TCL_LISTA, TCL_LEIDA.  
;.................................................................................

Tarea_Teclado:  tst Cont_Reb
                bne Retorno
                jsr Mux_Teclado
                brset Tecla,$FF,ProcesaTecla
                brset Banderas+1,$02,Tecla_Leida    ; Revisa que la tecla este leida
                movb Tecla, Tecla_IN
                bset Banderas+1, $02                  ; Pone la tecla como leida
                movb #$0A,Cont_Reb                 ; Pone 10 al contador de rebotes
                bra Retorno
Tecla_Leida:    ldaa Tecla
                cmpa Tecla_IN
                beq Tecla_Lista                
                movb #$FF,Tecla
                movb #$FF,Tecla_IN
                bclr Banderas+1,$03       ; Borra las banderas TCL_LISTA y TCL_LEIDA
                bra Retorno
Tecla_Lista:    bset Banderas+1, $01      ; Pone la bandera TCL_LISTA en 1
                bra Retorno

ProcesaTecla:   brset Banderas+1,$01,Guardar_Tecla  ; Si la tecla esta lista la guarda
                bra Retorno

Guardar_Tecla:  bclr Banderas+1,$03       ; Borra las banderas TCL_LISTA y TCL_LEIDA
                jsr Formar_Array
Retorno:        rts

;.......................................................................................
;                         SUBRUTINA MUX_TECLADO
;.......................................................................................
; Esta subrutina revisa constantemente el teclado para ver si una tecla fue presionada
;  haciendo uso de las variables Tecla y Tecla_IN.
;.......................................................................................

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
;  Esta subritina se encarga de guardar en el arreglo Num_Array las teclas 
; ingresadas por el usuario e ignorar las teclas que no correspondan
; o en el momento en que no correspondan.
;
;.................................................................................

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

Enter:          bset Banderas+1,$04    ; Cuano se da enter Array_OK pasa a 1
                clr Cont_TCL            
                bra Retorno_Array

Borrar:         dec Cont_TCL 
                ldab Cont_TCL
                movb #$FF,b,y         ; Llena los espacios borrados con FF
Retorno_Array:  rts

