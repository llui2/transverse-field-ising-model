C     SPIN MODEL MODULE
C     0!
C     Lluís Torres 
C     TFG
C     FORTRAN 95

      MODULE MODEL 
C     VIANA-BRAY SPIN-GLASS MODEL WITH DISCRETE DISTRIBUTION OF THE COUPLING IN A TRANSVERSE FIELD

C     MULTI ARRAY TYPE
      TYPE :: MULTI_ARRAY
      INTEGER,ALLOCATABLE :: v(:)
      END TYPE MULTI_ARRAY

      CONTAINS

C-----------------------------------------------------------------------
C     METROPOLIS.F
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     READ INPUT FILE
      SUBROUTINE READ_INPUT(L,m,TEMP_SIZE,TEMP_LIST,H_SIZE,H_LIST,
     .                        SEED,MCTOT,SC)

      INTEGER L
      INTEGER m
      INTEGER TEMP_SIZE
      REAL*8,ALLOCATABLE:: TEMP_LIST(:)
      INTEGER H_SIZE
      REAL*8,ALLOCATABLE:: H_LIST(:)
      INTEGER SEED
      INTEGER MCTOT
      INTEGER SC

      OPEN(UNIT=0,FILE="input.txt")
      
      READ(0,*)
      READ(0,*) L
      READ(0,*)
      READ(0,*) m
      READ(0,*)
      READ(0,*) TEMP_SIZE
      ALLOCATE(TEMP_LIST(1:TEMP_SIZE))
      READ(0,*) TEMP_LIST
      READ(0,*)
      READ(0,*) H_SIZE
      ALLOCATE(H_LIST(1:H_SIZE))
      READ(0,*) H_LIST
      READ(0,*)
      READ(0,*) SEED
      READ(0,*)
      READ(0,*) MCTOT
      READ(0,*)
      READ(0,*) SC
      
      CLOSE(0)

      RETURN
      END SUBROUTINE READ_INPUT
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     SPIN SYSTEM STRUCTURE: TORUS (SQUARE LATTICE WITH PBC)
      SUBROUTINE SQUARE_LATTICE_PBC(L,NBR,JJ)
      
      INTEGER L
      TYPE(MULTI_ARRAY),ALLOCATABLE :: NBR(:)
      TYPE(MULTI_ARRAY),ALLOCATABLE :: JJ(:)

      INTEGER i, j
      INTEGER iSPIN
      INTEGER UP, DOWN, RIGHT, LEFT
      INTEGER PBC(0:1,L)
      
      DO i=1,L
            PBC(0,i) = i-1
            PBC(1,i) = i+1
      END DO
      PBC(0,1) = L
      PBC(1,L) = 1
      ALLOCATE(NBR(L*L))
      ALLOCATE(JJ(L*L))
      DO i=1,L*L
            ALLOCATE(NBR(i)%v(0))
            ALLOCATE(JJ(i)%v(0))
      END DO
      iSPIN = 1
      DO j=1,L
            DO i=1,L
                  UP = i+L*(PBC(1,j)-1)
                  DOWN = i+L*(PBC(0,j)-1)
                  RIGHT = PBC(1,i)+L*(j-1)
                  LEFT = PBC(0,i)+L*(j-1)
                  CALL ADD_ELEMENT(NBR(iSPIN)%v,UP)
                  CALL ADD_ELEMENT(JJ(iSPIN)%v,1)
                  CALL ADD_ELEMENT(NBR(iSPIN)%v,DOWN)
                  CALL ADD_ELEMENT(JJ(iSPIN)%v,1)
                  CALL ADD_ELEMENT(NBR(iSPIN)%v,RIGHT)
                  CALL ADD_ELEMENT(JJ(iSPIN)%v,1)
                  CALL ADD_ELEMENT(NBR(iSPIN)%v,LEFT)
                  CALL ADD_ELEMENT(JJ(iSPIN)%v,1)
                  iSPIN = iSPIN+1
            END DO
      END DO

      END SUBROUTINE
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      REAL*8 FUNCTION ENERG(N,m,S,TEMP,H,NBR,JJ)
C     THIS FUNCTION CALCULATES THE ENERGY OF THE SYSTEM GIVEN A CONFIGURATION

      INTEGER N, m
      INTEGER S(1:m,1:N)
      REAL*8 TEMP, H
      TYPE(MULTI_ARRAY),ALLOCATABLE:: NBR(:)
      TYPE(MULTI_ARRAY),ALLOCATABLE:: JJ(:)

      INTEGER i, j, k
      REAL*8 K2
      REAL*8 HD, V
      INTEGER ABOVE(1:m) !SPIN ABOVE i

      DO i=1,m-1
            ABOVE(i) = i+1
      END DO
      ABOVE(m) = 1

      K2 = -(TEMP/2)*LOG(TANH(H/(TEMP*m)))

      HD = 0.0d0 !DIAGONAL TERM
      V = 0.0d0 !TRANSVERSE FIELD TERM

      DO i = 1,m
            DO j = 1,N
                  DO k = 1,SIZE(NBR(j)%v)
                        HD = HD + JJ(j)%v(k)*S(i,j)*S(i,NBR(j)%v(k))
                  END DO
                  V = V + S(i,j)*S(ABOVE(i),j)
            END DO
      END DO

      ENERG =  - HD/(2*m) - K2*V
      
      RETURN
      END FUNCTION ENERG
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     METROPOLIS ALGORITHM 
      SUBROUTINE METROPOLIS(N,m,S,valid,TEMP,H,DE,NBR,JJ)
C     THIS SUBROUTINE PROPOSES A CHANGE OF SPIN IN A RANDOM NODE, CALCULATES
C     THE ENERGY VARIATION OF THE SYSTEM (ΔH_eff) DUE TO IT, IF ΔH_eff < 0
C     THEN THE CHANGE IS ACCPETED, ELSE IF ΔH_eff > 0 THEN THE CHANGE IS 
C     ACCEPTED WITH A PROBABILITY OF EXP(-ΔH_eff/k_BT).

      INTEGER N, m
      INTEGER S(1:m,1:N)
      LOGICAL valid
      REAL*8 TEMP, H, DE
      TYPE(MULTI_ARRAY),ALLOCATABLE:: NBR(:)
      TYPE(MULTI_ARRAY),ALLOCATABLE:: JJ(:)

      EXTERNAL r1279
      REAL*8 DHD, DV
      REAL*8 K2

      INTEGER PBC(0:m+1) !PBC IN THE TROTTER DIRECTION

      PBC(0)=m
      DO i=1,m
            PBC(i) = i
      END DO
      PBC(m+1) = 1

      valid = .FALSE.

C     RANDOM NODE SELECTION
      i = INT(r1279()*m) + 1
      j = INT(r1279()*N) + 1
      
C     CALCULATION OF ΔHD, COUPLINGS CONTRIBUTION
      DHD = 0
      DO k=1,SIZE(NBR(j)%v)
            DHD = DHD + JJ(j)%v(k)*S(i,NBR(j)%v(k))
      END DO
      DHD = 2*DHD*S(i,j)/m

C     CALCULATION OF ΔV, TRANSVERSE CONTRIBUTION
      K2 = -(TEMP/2.)*LOG(TANH(H/(TEMP*m)))
      DV = K2*S(i,j)*(S(PBC(i+1),j)+S(PBC(i-1),j))

C     CALCULATION OF ΔH
      DE = DHD + DV

C     CHECK IF THE CHANGE IS ACCEPTED
      IF (r1279().LT.min(1.d0,exp(-DE/TEMP))) THEN
            S(i,j) = -S(i,j)
            valid = .true.
      END IF

      RETURN
      END SUBROUTINE METROPOLIS
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      REAL*8 FUNCTION MAGNET_Z(N,m,S)

      INTEGER N, m
      INTEGER S(1:m,1:N)

      INTEGER i, j
      REAL*8 SUM
      
      SUM = 0.D0
      DO i = 1,m
            DO j = 1,N
                  SUM = SUM + S(i,j)
            END DO
      END DO

      MAGNET_Z = SUM/(N*m)

      RETURN
      END FUNCTION MAGNET_Z
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      REAL*8 FUNCTION MAGNET_X(N,m,TEMP,H,S)

      INTEGER N, m
      REAL*8 TEMP, H
      INTEGER S(1:m,1:N)

      INTEGER i, j
      REAL*8 SUM
      INTEGER PBC(0:m+1) !PBC IN THE TROTTER DIRECTION
      REAL*8 K

      PBC(0)=m
      DO i=1,m
            PBC(i) = i
      END DO
      PBC(m+1) = 1

      K = TANH(H/(m*TEMP)) 

      SUM = 0.D0
      DO i = 1,m
            DO j = 1,N
                  SUM = SUM + K**(S(i,j)*S(PBC(i+1),j))
            END DO
      END DO

      MAGNET_X = SUM/(N*m)

      RETURN
      END FUNCTION MAGNET_X
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE ADD_ELEMENT(LIST, ELEMENT)
C     ADD ELEMENT INTO LIST

      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST
      INTEGER ELEMENT

      INTEGER, DIMENSION(:), ALLOCATABLE :: CLIST
      INTEGER i, iSIZE

      IF (ALLOCATED(LIST)) THEN 
            ISIZE=SIZE(LIST)
            ALLOCATE(CLIST(iSIZE+1))
            DO i=1,iSIZE          
            CLIST(i) = LIST(i)
            END DO
            CLIST(iSIZE+1) = ELEMENT
            DEALLOCATE(LIST)
            CALL move_alloc(CLIST, LIST)
      ELSE
            ALLOCATE(LIST(1))
            LIST(1) = ELEMENT
      END IF

      RETURN
      END SUBROUTINE
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     REMOVE INDEX FROM LIST
      SUBROUTINE RMVOFLIST(list,index)

      INTEGER, DIMENSION(:), ALLOCATABLE:: list
      INTEGER index

      INTEGER i, isize
      INTEGER, DIMENSION(:), ALLOCATABLE:: clist

      IF (ALLOCATED(list)) THEN
            isize = SIZE(list)
            ALLOCATE(clist(isize-1))
            DO i = 1,index-1
                  clist(i) = list(i)
            END DO
            DO i = index,isize-1
                  clist(i) = list(i+1)
            END DO
            DEALLOCATE(list)
            CALL MOVE_ALLOC(clist, list)

      END IF

      RETURN
      END SUBROUTINE RMVOFLIST
C-----------------------------------------------------------------------

      END MODULE MODEL