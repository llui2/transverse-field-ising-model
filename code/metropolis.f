C     METROPOLIS SAMPLE GENERATOR
C     0!
C     Lluís Torres 
C     TFM
C     FORTRAN 95

      PROGRAM SAMPLE_GENERATOR

      USE MODEL

C-----(SYSTEM)------------------------------------------------
C     LINEAR SIZE, NUMBER OF SPINS
      INTEGER L, N
C     TROTTER INDEX
      INTEGER m
C     TEMPERATURE (TEMP := k_B·T)
      REAL*8 TEMP
C     H (:= Γ = TRANSVERSE FIELD)
      REAL*8 H
C     p-LIST, TEMP-LIST, Γ-LIST
      INTEGER TEMP_SIZE, H_SIZE
      REAL*8,ALLOCATABLE:: TEMP_LIST(:), H_LIST(:)
C     PRINCIPAL ARRAYS
      TYPE(MULTI_ARRAY),ALLOCATABLE:: NBR(:)
      TYPE(MULTI_ARRAY),ALLOCATABLE:: JJ(:)
C-----(SIMULATION)---------------------------------------------
C     TOTAL MONTE-CARLO STEPS (MCS)
      INTEGER MCTOT
C     MCS TILL WE CONSIDER EQUILIBRIUM
      INTEGER MCINI
C     SAVE SPIN CONFIGURATIONS EVERY SC (MCS)
      INTEGER SC
C     SEED NUMBER
      INTEGER SEED
C     RANDOM NUMBER GENERATOR
      EXTERNAL r1279
C     ESTIMATE TIME VARIABLES
      REAL*4 TIME1, TIME2, time
C     SIMULATION VARIABLES
      INTEGER, ALLOCATABLE:: S(:,:)
      LOGICAL valid
      REAL*8 DE
      REAL*8 ENE
C-----(DUMMY)-------------------------------------------------
      INTEGER ITEMP, IH
      INTEGER IMC
      CHARACTER(4) str
      CHARACTER(3) str1, str2
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C-----------------------------------------------------------------------
C     START
C-----------------------------------------------------------------------

      PRINT*, 'METROPOLIS'

C***********************************************************************
C     READ SIMULATION VARIABLES FROM INPUT FILE
      CALL READ_INPUT(L,m,TEMP_SIZE,TEMP_LIST,H_SIZE,H_LIST,
     .                        SEED,MCTOT,SC)
      N = L**2
      MCINI = 0!MCTOT/3
C     ALLOCATION
      ALLOCATE(S(1:m,1:N))
C***********************************************************************
      CALL CPU_TIME(TIME1)
C***********************************************************************
C     SYSTEM STRUCTURE
      CALL SQUARE_LATTICE_PBC(L,NBR,JJ)
C***********************************************************************
C     CREATE DIRECTORY FOR EACH TEMP AND Γ VALUE IN THE 'sample' FOLDER
      CALL SYSTEM('mkdir -p results/sample/')
C     COPY INPUT FILE TO THE RESULTS FOLDER
      CALL SYSTEM('cp input.txt results/input.txt')
C***********************************************************************

C     FOR ALL TEMP VALUES
      DO ITEMP = 1,TEMP_SIZE
      TEMP = TEMP_LIST(ITEMP)
      WRITE(str,'(f4.2)') TEMP
      str1 = str(1:1)//str(3:4)

C     FOR ALL Γ VALUES      
      DO IH = 1,H_SIZE
      H = H_LIST(IH)
      WRITE(str,'(f4.2)') H
      str2 = str(1:1)//str(3:4)

C***********************************************************************
C     INITIALIZE RANDOM NUMBER GENERATOR
      CALL setr1279(SEED)
C***********************************************************************
C     SPIN CONFIGURATION FILE FOR EACH p VALUE AND SEED
      OPEN(UNIT=1,FILE='results/sample/T'//str1//'_Γ'//str2//'.dat')
      WRITE(1,'(g0)') 'MCS,Mz,Mx,Ene'
C***********************************************************************
C     GENERATION OF TWO RANDOM INITIAL SPIN CONFIGURATIONS
      DO i = 1,m
            DO j = 1,N
                  S(i,j) = INT(2*MOD(INT(2*r1279()),2) - 1)
            END DO
      END DO
C***********************************************************************
C     INITIAL ENERGY
      ENE = ENERG(N,m,S,TEMP,H,NBR,JJ)
C***********************************************************************
C     MONTE-CARLO SIMULATION
      DO IMC = 1,MCTOT
            DO IPAS = 1,N*m
                  CALL METROPOLIS(N,m,S,valid,TEMP,H,DE,NBR,JJ)
                  IF (valid) THEN
                        ENE = ENE + DE
                  END IF
            END DO
C           EXTRACT THERMODYNAMICS EVERY SC MONTE-CARLO STEPS
            IF ((IMC.GT.MCINI).AND.(SC*(IMC/SC).EQ.IMC)) THEN
200         FORMAT(I0,3(A,F0.8))
            WRITE(1,200) IMC,',',ABS(MAGNET_Z(N,m,S)),',',
     .       MAGNET_X(N,m,TEMP,H,S),',',ENE/N
            END IF
      END DO !IMC
C***********************************************************************
      CLOSE(1)

C***********************************************************************
C     ESTIMATE EXECUTION TIME
300   FORMAT (A,I4,A,I3,A,I3,A,I3,A,I3,A,I3)
      IF ((TEMP.EQ.TEMP_LIST(1)).AND.(H.EQ.H_LIST(1))) THEN
      CALL CPU_TIME(TIME2)
      time = (TIME2-TIME1)*H_SIZE*TEMP_SIZE
      WRITE(*,300) "ESTIMATED TIME: ", INT(time/3600), ' h',
     . INT((time/3600-INT(time/3600))*60), ' min', 
     . INT((time/60-INT(time/60))*60), ' s'
      END IF
C***********************************************************************

      END DO !IH
      END DO !ITEMP

C***********************************************************************
      CALL CPU_TIME(TIME2)
      time = (TIME2-TIME1)
      WRITE(*,300) "CPU TIME: ", INT(time/3600), ' h',
     . INT((time/3600-INT(time/3600))*60), ' min', 
     . INT((time/60-INT(time/60))*60), ' s'
C***********************************************************************

C     DELLOCATE ARRAYS
      DO i = 1,N
            DEALLOCATE(NBR(i)%v)
            DEALLOCATE(JJ(i)%v)
      END DO
      DEALLOCATE(NBR)
      DEALLOCATE(JJ)

      END PROGRAM SAMPLE_GENERATOR