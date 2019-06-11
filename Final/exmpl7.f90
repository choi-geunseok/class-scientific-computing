!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM EXMPL7 
!     Example 7: The time-dependent Schroedinger Equation               
!  COMPUTATIONAL PHYSICS (FORTRAN VERSION)                              
!  by Steven E. Koonin and Dawn C. Meredith                             
!  Copyright 1989, Addison-Wesley Publishing Company, Inc.              
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                          
      CALL INIT       !display header screen, setup parameters        
      !5 CONTINUE     !main loop/ execute once for each set of param
      CALL PARAM    !get input from screen                        
      CALL ARCHON   !calculate time evolution of a wavepacket     
      !GOTO 5 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ARCHON 
! calculates the time evolution of a one-dimensional wavepacket         
! according to the time-dependent Schroedinger equation                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
      INCLUDE 'IO.ALL' 
! Local variables:                                                      
      COMPLEX PHI(0:MAXLAT)    !wavepacket                               
      REAL PHI2(0:MAXLAT)      !wavepacket squared                    
      REAL TIME                                                        
      REAL TPROB,LPROB,RPROB   !normalization of the wavepacket       
      REAL LX,RX,TX            !average position                      
      REAL E                   !energy                                
      REAL DT                  !time step                             
      INTEGER IT               !time index                            
      REAL DTSCAL              !scaled time step                      
      REAL DTMIN,DTMAX         !min,max reasonable size for DTSCAL    
      INTEGER NLINES           !number of lines printed to terminal   
      INTEGER SCREEN           !send to terminal                      
      INTEGER PAPER            !make a hardcopy                       
      INTEGER FILE             !send to a file                        
      INTEGER NSTEP            !number of time steps to take          
      LOGICAL MORE,NEWDT       !options for continuing                
      INTEGER FRAME            !which frame of the movie is it?       
      INTEGER NFREQ            !graphing frequency (movies only)      
      REAL DIFF                !temp variables for movies             
      INTEGER EL                                                        !
! Functions:                     Evolution loop                         
      REAL GETFLT              !get floating point number from screen 
      INTEGER YESNO            !get yes/no answer from screen         
      LOGICAL LOGCVT           !change from 1,0 to true and false     
      INTEGER GETINT           !get integer data from screen          
      DATA SCREEN,PAPER,FILE/1,2,3/ 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     output summary of parameters                                      
      IF (TTERM) CALL PRMOUT(OUNIT,NLINES) 
      IF (TFILE) CALL PRMOUT(TUNIT,NLINES) 
      IF (GFILE) CALL PRMOUT(GUNIT,NLINES) 
                                
      CALL INTPHI(PHI,PHI2) !setup initial PHI array                
      TIME=0.               !initialize time                        
      !NSTEP=40             !def num of time steps until next prompt
      NFREQ=10              !!def graphing freq (movies only)        
      !DTSCAL=.2            !!def scaled time step                   
      !DTMAX=100.           !!max scaled time step                   
     OPEN(10, FILE='INP', FORM='FORMATTED') 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*) 
     READ(10,*)NSTEP
     READ(10,*)DTSCAL 
     READ(10,*)DTMAX 
     CLOSE(10)
     DTMIN=DX**2*K0**2/25. !minimum scaled time step               
!     OPEN(10, FILE='INPUT', FORM='FORMATTED') 
!     READ(10,*) ET 
!     CLOSE(10)     
!     DO  EL=1,ET 
      !    DO  IT=1,NSTEP 
      !10 CONTINUE 
        !print*, 'Enter time step (units of K0**-2)'
        !read(*,*) DTSCALE 
        !DTSCAL=GETFLT(DTSCAL,-DTMAX,DTMAX,                 &
     !&      'Enter time step (units of K0**-2)')                        
        IF (ABS(DTSCAL) .LT. DTMIN)      &    !don't let it get too small               
            DTSCAL=SIGN(DTMIN,DTSCAL)    !physical time                                                
        DT=DTSCAL/K0**2 
        !NSTEP=GETINT(NSTEP,1,1000,'Enter number of time steps') 
        !print*, 'Enter number of time steps'
        !read(*,*) NSTEPS
        NLINES=NLINES+4 
        IF (MOVIES) THEN 
           !NFREQ=GETINT(NFREQ,1,1000,'Enter graphing frequency') 
           !print*, read(*,*) NFREQ
           !make sure that total num of frames is divisible by 4        
           DIFF=MOD(NSTEP,4*NFREQ) 
           IF (DIFF .NE. 0) NSTEP=NSTEP+4*NFREQ-DIFF 
        END IF 
        CALL TRDIAG(DT) !calculate GAMMA for this DT           
        IF (TFILE) CALL TITLES(TUNIT,NLINES,DT) 
!15   CONTINUE    !loop over sets of NSTEP time steps    
          IF (TTERM) CALL TITLES(OUNIT,NLINES,DT) 
          DO  IT=1,NSTEP      !time evolution                 
             TIME=TIME+DT     !take a time step  
             CALL EVOLVE(PHI,PHI2,DT) 
             CALL NORMLZ(PHI2,LPROB,RPROB,TPROB,LX,RX,TX) 
             CALL ENERGY(PHI,PHI2,E) 
             !output                                                    
             IF ((MOVIES) .AND. (MOD(IT,NFREQ) .EQ. 0)) THEN 
                FRAME=MOD(IT/NFREQ,4) 
                IF (FRAME .EQ. 1) THEN 
!                    CALL GRFOUT(SCREEN,PHI2,TIME,TPROB,TX,E) ! Original
                     CALL GRFOUT(PHI,PHI2,TIME,TPROB,TX,E)  ! Redefined MEK:2019.06.11 
                ELSE 
                    CALL GRFSEC(SCREEN,PHI2,TIME,TPROB,TX,FRAME) 
                END IF 
             END IF 
             IF (TTERM) CALL                                            &
     &        TXTOUT(OUNIT,E,TIME,LPROB,RPROB,TPROB,LX,RX,TX,NLINES)    
             IF (TFILE) CALL                                            &
     &        TXTOUT(TUNIT,E,TIME,LPROB,RPROB,TPROB,LX,RX,TX,NLINES)    
!   20     CONTINUE 
!         graphics output                                               
          !IF (MOVIES) THEN 
             !CALL TMODE 
          !ELSE IF (GTERM) THEN 
!            print out graphics now if text was being sent to screen    
             !CALL PAUSE('to see the wave packet ...',1) 
             !PAUSE('to see the wave packet ...') 
             !CALL GRFOUT(SCREEN,PHI2,TIME,TPROB,TX,E) 
          !END IF 
         IF (GFILE) CALL GRFOUT(PHI,PHI2,TIME,TPROB,TX,E)         ! Redefined MEK:2019.06.11 
!        IF (GFILE) CALL GRFOUT(DEVICE,PHI,PHI2,TIME,TPROB,TX,E)  ! Redefined MEK:2019.06.11 
!        IF (GHRDCP) CALL GRFOUT(PAPER,PHI2,TIME,TPROB,TX,E) !Original 
         IF (GFILE) CALL GRFOUT(PHI,PHI2,TIME,TPROB,TX,E)         ! Redefined MEK:2019.06.11 
          !MORE=LOGCVT(YESNO(1,'Continue iterating?')) 
          MORE= .TRUE.  
        !IF (MORE) THEN 
            !NLINES=0 
            !IF (TTERM) system('clear') 
            !GOTO 15 
        !END IF 
        !NEWDT=LOGCVT(YESNO(1,'Change time step and continue?')) 
        NEWDT= .TRUE. 
      IF (NEWDT) THEN 
        NLINES=0 
        !IF (TTERM) system('clear')
!GOTO 10 
      END IF 
     END DO
      RETURN 
      END                                           
!-----------------------------------------------------------------------
      SUBROUTINE EVOLVE(PHI,PHI2,DT) 
! take one time step using the implicit algorithm                       
! (Eq. 7.30-7.33 and 7.11-7.16)                                         
!-----------------------------------------------------------------------
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
! Input/output variables:                                               
      COMPLEX PHI(0:MAXLAT)      !wavepacket (I/O)                       
      REAL PHI2(0:MAXLAT)        !wavepacket squared (I/O)              
      REAL DT                    !time step (input)                     
! Local variables:                                                       
      COMPLEX CONST              !term in matrix inversion              
      COMPLEX CHI                !part of wave function                 
      COMPLEX BETA(0:MAXLAT)     !term in matrix inversion              
      INTEGER IX                 !lattice index                         
!-----------------------------------------------------------------------
      CONST=4*SQRTM1*DX*DX/DT 
      BETA(NPTS-1)=0.           !initial conditions for BETA              
      DO  IX=NPTS-2,0,-1      !backward recursion for BETA           
         BETA(IX)=GAMMA(IX+1)*(BETA(IX+1)-CONST*PHI(IX+1)) 
      END DO 
      CHI=(0.,0.)              !boundary conditions                          
      DO IX=1,NPTS-1        !forward recursion for CHI and PHI     
         CHI=GAMMA(IX)*CHI+BETA(IX-1)    !CHI at this lattice point        
         PHI(IX)=CHI-PHI(IX)             !PHI at new time                 
         PHI2(IX)=ABS(PHI(IX))**2        !PHI2 at new time                
      END DO 
      RETURN 
      END                                           
!-----------------------------------------------------------------------
      SUBROUTINE TRDIAG(DT) 
! calculate GAMMA for the inversion of the tridiagonal matrix           
! (Eq. 7.30-7.33 and 7.11-7.16)                                         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
! Input variables:                                                      
      REAL DT                !time step                       
! Local variables:                                                       
      INTEGER IX             !lattice index                   
      COMPLEX AZERO,CONST1   !terms in matrix inversion       
      REAL CONST2            !useful constant                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CONST1=-2.+2*SQRTM1*DX**2/DT 
      CONST2=DX*DX*K0*K0 
      GAMMA(NPTS)=0.                   !initial conditions for GAMMA     
      DO  IX=NPTS-1,0,-1             !backward recursion              
         AZERO=CONST1-CONST2*V(IX)     !use GAMMA(IX)=ALPHA(IX-1)       
         GAMMA(IX)=-1./(AZERO+GAMMA(IX+1)) 
      END DO 
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE NORMLZ(PHI2,LPROB,RPROB,TPROB,LX,RX,TX) 
! given PHI2, finds left, right, and total probability of the wavepacket
! as well as the average X value (left, right, and total)               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
! Input variable:                                                       
      REAL PHI2(0:MAXLAT)     !wavepacket squared                     
! Output variables:                                                      
      REAL LPROB,RPROB,TPROB  !left, right, and total probability     
      REAL LX,RX,TX           !left, right, and total average position
! Local variables:                                                       
      INTEGER IX              !lattice index                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      LPROB=0.                        !zero sums                     
      LX=0.                                                           
      DO  IX=1,IMID-1               !integrate with trapezoidal rul
         LPROB=LPROB+PHI2(IX)                                         
         LX=LX+X(IX)*PHI2(IX)                                         
      END DO                                                          
      LPROB=LPROB+PHI2(IMID)/2        !middle point is shared between
      LX=LX+XMID*PHI2(IMID)/2         !  left and right              
      RPROB=PHI2(IMID)/2              !middle point contribution     
      RX=XMID*PHI2(IMID)/2                                            
      DO  IX=IMID+1,NPTS-1          !integrate with trapezoidal rul
         RPROB=RPROB+PHI2(IX)                                         
         RX=RX+X(IX)*PHI2(IX)                                         
      END DO                                                          
      TPROB=LPROB+RPROB               !total probability             
      IF (LPROB .NE. 0.) LX=LX/LPROB  !normalize LX,RX               
      IF (RPROB .NE. 0.) RX=RX/RPROB                                  
      TX=LX*LPROB+RX*RPROB            !total <x> is a weighted sum   
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ENERGY(PHI,PHI2,E) 
! calculate the scaled energy E (units of K0**2) given PHI and PHI2     
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
! Input/Output variables:                                               
      COMPLEX PHI(0:MAXLAT)   !wavepacket (input)                     
      REAL PHI2(0:MAXLAT)     !wavepacket squared (input)            
      REAL E                  !total scaled energy (output)          
! Local variables:                                                       
      INTEGER IX              !lattice index                         
      REAL PE                 !potential energy                      
      COMPLEX KE              !kinetic energy                        
      REAL A_real, A_imag 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PE=0.                      !initialize sums                       
      KE=0.                                                              
      DO  IX=1,NPTS-1          !integrate using trapezoidal rule      
         PE=PE+V(IX)*PHI2(IX) 
         KE=KE+CONJG(PHI(IX))*(PHI(IX-1)-2*PHI(IX)+PHI(IX+1)) 
      END DO 
      KE=-KE/DX**2/K0**2 
      E=PE+REAL(KE)              !energy scale = K0**2                  
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE INTPHI(PHI,PHI2) 
! creates initial PHI array from input parameters                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
! Output variables:                                                     
      COMPLEX PHI(0:MAXLAT)         !wavepacket                           
      REAL PHI2(0:MAXLAT)           !wavepacket squared                  
! Local variables:                                                       
      INTEGER IX                    !lattice index                       
      REAL NORM,LPROB,RPROB,SQRTN   !normalization of the wavepacket     
      REAL LX,RX,TX                 !average position                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!Lorentzian                  
      IF (PACKET .EQ. LORNTZ) THEN 
         DO  IX=1,NPTS-1 
            PHI(IX)=CEXP(SQRTM1*K0*X(IX))/(SIGMA**2+(X(IX)-W0)**2) 
            PHI2(IX)=CABS(PHI(IX))**2 
         END DO 
!Gaussian                    
      ELSE IF (PACKET .EQ. GAUSS) THEN 
         DO  IX=1,NPTS-1 
            PHI(IX)=CEXP(SQRTM1*K0*X(IX))*EXP(-(X(IX)-W0)**2/2/SIGMA**2) 
            PHI2(IX)=CABS(PHI(IX))**2 
         END DO 
!User defined wavepacket                    
      ELSE IF (PACKET .EQ. USER) THEN 
           OPEN(66, FILE='wavepacket.txt', FORM='FORMATTED') 
              DO  IX=1,NPTS 
                READ (66,*) A_real,A_imag
                PHI(IX) = CMPLX(A_real,A_imag) 
                PHI2(IX)=CABS(PHI(IX))**2 
!                print*, PHI(IX) 
              ENDDO
           CLOSE(66)
      END IF 
      PHI(0)=0.      !potential is infinite at ends of lattice    
      PHI(NPTS)=0.   ! so PHI is zero there                      
                                                  
      CALL NORMLZ(PHI2,LPROB,RPROB,NORM,LX,RX,TX) !normalize wavepacket 
      SQRTN=SQRT(NORM) 
      DO  IX=0,NPTS 
         PHI(IX)=PHI(IX)/SQRTN 
         PHI2(IX)=PHI2(IX)/NORM 
      END DO 
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE INIT 
! initializes constants, displays header screen,                        
! initializes menu arrays for input parameters                          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'IO.ALL' 
      INCLUDE 'MENU.ALL' 
      INCLUDE 'PARAM.E7' 
! Local parameters:                                                     
      CHARACTER*80 DESCRP           !program description                  
      DIMENSION DESCRP(20)                                              
      INTEGER NHEAD,NTEXT,NGRAPH    !number of lines for each description
      REAL EPS                      !small number to set width of potntls
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     get environment parameters                                        
      !CALL SETUP 
!     display header screen                                             
      DESCRP(1)= 'EXAMPLE 7' 
      DESCRP(2)= 'Solution of time-dependent Schroedinger Equation' 
      DESCRP(3)= 'for a wavepacket in a one-dimensional potential' 
      NHEAD=3 
!     text output description                                           
      DESCRP(4)= 'time, energy, probability, and <x>' 
      NTEXT=1 
!     graphics output description                                       
      DESCRP(5)= 'potential and |wavepacket|**2 vs. x' 
      NGRAPH=1 
      !CALL HEADER(DESCRP,NHEAD,NTEXT,NGRAPH) 
!     define constants                                                  
      PI=4.0*ATAN(1.0) 
      SQRTM1=(0,1) !square root of -1                      
!     if you change VXMIN and/or VXMAX, change prompts below            
!     for X0, W0, and CYCLE=K0*(VXMAX-VXMIN)/2/PI                       
!physical limits on lattice             
      VXMIN=-1. 
      VXMAX=1.
      EPS=1./20.                  !find constants so that at X=X0+A        
      AGAUSS=LOG(1./EPS)          ! or X=XO-A, V(X)=EPS*V0                
      ASTEP=TAN(PI*(1./2.-EPS))   !(doesn't apply to parabola)            
!     setup menu arrays, beginning with constant part                   
      !MENU 
      MTYPE(13)=TITLE 
      MPRMPT(13)='POTENTIAL FUNCTION MENU' 
      MLOLIM(13)=2. 
      MHILIM(13)=1. 
      MTYPE(14)=MTITLE 
      MPRMPT(14)='1) Square-well: V=V0 for X0-A < X < X0+A'           
      MLOLIM(14)=0. 
      MHILIM(14)=0. 
      
      MTYPE(15)=MTITLE 
      MPRMPT(15)='2) Gaussian: V(X)=V0*(EXP-(AGAUSS*((X-X0)/A)**2))'   
      MLOLIM(15)=0. 
      MHILIM(15)=0. 
      
      MTYPE(16)=MTITLE 
      MPRMPT(16)='3) Parabolic: V(X)=V0*(X-X0)**2/A**2'                
      MLOLIM(16)=0. 
      MHILIM(16)=0. 
      
      MTYPE(17)=MTITLE 
      MPRMPT(17)='4)Smooth step: V(X)=V0*(2/PI*ATN(ASTEP*(X-X0)/A)+1)/2'
      MLOLIM(17)=0. 
      MHILIM(17)=1. 
      
      MTYPE(18)=MCHOIC 
      MPRMPT(18)='Make a menu choice and press return' 
      MTAG(18)='19 19 19 19' 
      MLOLIM(18)=1. 
      MHILIM(18)=4. 
      MINTS(18)=1 
      MREALS(18)=1. 
      
      MTYPE(19)=FLOAT 
      MPRMPT(19)='Enter X0 (center of potential  -1 < X0 < 1 )' 
      MTAG(19)='Center of potential (X0)' 
      MLOLIM(19)=VXMIN 
      MHILIM(19)=VXMAX 
      MREALS(19)=VXMIN+3*(VXMAX-VXMIN)/4 
      
      MTYPE(20)=FLOAT 
      MPRMPT(20)='Enter A (half width of one-twentieth max)' 
      MTAG(20)='Half-width of potential (A)' 
      MLOLIM(20)=10*(VXMAX-VXMIN)/MAXLAT 
      MHILIM(20)=(VXMAX-VXMIN)*10. 
      MREALS(20)=(VXMAX-VXMIN)/25. 
      
      MTYPE(21)=FLOAT 
      MPRMPT(21)='Enter V0 (height of potential in units of K0**2)' 
      MTAG(21)='Height of potential V0 (units of K0**2)' 
      MLOLIM(21)=-100. 
      MHILIM(21)=100. 
      MREALS(21)=.3 
      
      MTYPE(22)=FLOAT 
      MPRMPT(22)=     &
     & 'Enter XMID which separates right from left (-1 < XMID < 1)'     
      MTAG(22)='middle X value' 
      MLOLIM(22)=VXMIN 
      MHILIM(22)=VXMAX 
      MREALS(22)=VXMIN+3*(VXMAX-VXMIN)/4 
      
      MTYPE(23)=TITLE 
      MPRMPT(23)='WAVEPACKET MENU' 
      MLOLIM(23)=2. 
      MHILIM(23)=1. 
      
      MTYPE(24)=MTITLE 
      MPRMPT(24)=    &
     &'1) Lorentzian:  PHI(X)=EXP(I*K0*X)/(SIGMA**2+(X-W0)**2)'         
      MLOLIM(24)=0. 
      MHILIM(24)=0. 
      
      MTYPE(25)=MTITLE 
      MPRMPT(25)=    &
     &'2) Gaussian: PHI(X)=EXP(I*K0*X)*EXP(-(X-W0)**2/2/SIGMA**2)'      
      MLOLIM(25)=0. 
      MHILIM(25)=1. 
      
      MTYPE(26)=MCHOIC 
      MPRMPT(26)='Make a menu choice and press return' 
      MTAG(26)='27 27' 
      MLOLIM(26)=1. 
      MHILIM(26)=2. 
      MINTS(26)=2 
      MREALS(26)=2. 
      
      MTYPE(27)=FLOAT 
      MPRMPT(27)='Enter W0 (center of packet -1 < W0 < 1 )' 
      MTAG(27)='Center of packet (W0)' 
      MLOLIM(27)=VXMIN 
      MHILIM(27)=VXMAX 
      MREALS(27)=VXMIN+(VXMAX-VXMIN)/3 
      
      MTYPE(28)=FLOAT 
      MPRMPT(28)='Enter SIGMA (width of packet)' 
      MTAG(28)='Width of packet (SIGMA)' 
      MLOLIM(28)=10*(VXMAX-VXMIN)/MAXLAT 
      MHILIM(28)=10*(VXMAX-VXMIN) 
      MREALS(28)=.125*(VXMAX-VXMIN) 
                                                                  
      MTYPE(29)=FLOAT 
      MPRMPT(29)='Enter number of cycles on lattice = K0/PI ' 
      MTAG(29)='Number of cycles = K0/PI' 
      MLOLIM(29)=-REAL(MAXLAT)/4. 
      MHILIM(29)= REAL(MAXLAT)/4. 
      MREALS(29)= 10. 
      
      MTYPE(30)=SKIP 
      MREALS(30)=35. 
                                                                  
      MTYPE(38)=NUM 
      MPRMPT(38)= 'Enter number of lattice points' 
      MTAG(38)= 'Number of lattice points' 
      MLOLIM(38)=20. 
      MHILIM(38)=MAXLAT 
      MINTS(38)=200 
      
      MTYPE(39)=SKIP 
      MREALS(39)=60. 
      
      MSTRNG(MINTS(75))= 'exmpl7.txt' 
                                                                  
      MTYPE(76)=SKIP 
      MREALS(76)=80. 
      MSTRNG(MINTS(86))= 'exmpl7.grf' 
      MTYPE(87)=SKIP 
      MREALS(87)=90. 
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE PARAM 
! gets parameters from screen                                           
! ends program on request                                               
! closes old files                                                      
! maps menu variables to program variables                              
! opens new files                                                       
! calculates all derivative parameters                                  
! performs checks on parameters                                         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'MENU.ALL' 
      INCLUDE 'IO.ALL' 
      INCLUDE 'PARAM.E7' 
! Local variables:                                                      
      REAL CYCLES          !number of cycles on lattice               
      REAL MAXCYC          !maximum number of cycles for given NPTS  
!     map between menu items and parameters                             
      INTEGER IPOT,IX0,IA,IV0,IXMID,IPACK,ISIGMA,IW0,ICYCLE,INPTS 
      PARAMETER (IPOT   = 18) 
      PARAMETER (IX0    = 19) 
      PARAMETER (IA     = 20) 
      PARAMETER (IV0    = 21) 
      PARAMETER (IXMID  = 22) 
      PARAMETER (IPACK  = 26) 
      PARAMETER (IW0    = 27) 
      PARAMETER (ISIGMA = 28) 
      PARAMETER (ICYCLE = 29) 
      PARAMETER (INPTS  = 38) 
! Functions:                                                            
      LOGICAL LOGCVT      !converts 1 and 0 to true and false       
      REAL GETFLT         !get floating point number from screen    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     get input from terminal                                           
      !system('clear') 
      !CALL ASK(1,ISTOP) 
!     stop program if requested                                         
      IF (MREALS(IMAIN) .EQ. STOP) stop
!     close files if necessary                                          
      !IF (TNAME .NE. MSTRNG(MINTS(ITNAME)))        &
     !&     CALL FLCLOS(TNAME,TUNIT)                   
      !IF (GNAME .NE. MSTRNG(MINTS(IGNAME)))        &
     !&     CALL FLCLOS(GNAME,GUNIT)                                     
!     set new parameter values                                          
!     physical and numerical                                           
     OPEN(10, FILE='INP', FORM='FORMATTED') 
     READ(10,*) POT
     READ(10,*) X0
     READ(10,*) A
     READ(10,*) V0
     READ(10,*) PACKET
     READ(10,*) W0
     READ(10,*) SIGMA
     READ(10,*) CYCLES
     READ(10,*) NPTS
     CLOSE(10)
     XMID=X0
      !print*, POT,NPTS
      !POT=MINTS(IPOT) 
      !X0=MREALS(IX0) 
      !A=MREALS(IA) 
      !V0=MREALS(IV0) 
      !XMID=MREALS(IXMID) 
      !PACKET=MINTS(IPACK) 
      !SIGMA=MREALS(ISIGMA) 
      !W0=MREALS(IW0) 
      !CYCLES=MREALS(ICYCLE) 
      !NPTS=MINTS(INPTS) 
      !NPTS=5 

!     text output                                                       
      !TTERM=LOGCVT(MINTS(ITTERM)) 
      !TFILE=LOGCVT(MINTS(ITFILE)) 
      !TNAME=MSTRNG(MINTS(ITNAME)) 
      TNAME=MSTRNG(MINTS(75)) 
      TTERM= .TRUE. 
      TFILE= .TRUE. 
      TUNIT =  6
      GUNIT =  7
      !print*, 'TTERM', TTERM      
      !print*, 'TFILE', TFILE      
      !print*, 'TNAME ', TNAME,ITNAME      
      !print*,'TUNIT',  TUNIT
      !print*, 'NPTS', NPTS

!     graphics output                                                   
      !GTERM=LOGCVT(MINTS(IGTERM)) 
      !GHRDCP=LOGCVT(MINTS(IGHRD)) 
      !GFILE=LOGCVT(MINTS(IGFILE)) 
      GFILE= .TRUE.
      !GNAME=MSTRNG(MINTS(IGNAME)) 
      !print*, 'GNAME ',GNAME  
      !print*, 'TNAME ',TNAME 

!     open files                                                        
      !IF (TFILE) CALL FLOPEN(TNAME,TUNIT) 
      !IF (TFILE)  OPEN(TUNIT, FILE=TNAME, FORM='FORMATTED') 
!      IF (TFILE)  OPEN(TUNIT, FILE='exmpl7.out1', FORM='FORMATTED') 
      IF (GFILE)  OPEN(GUNIT, FILE='OUT.txt', FORM='FORMATTED') 
      !IF (GFILE) CALL FLOPEN(GNAME,GUNIT) 
      !files may have been renamed                                      
      MSTRNG(MINTS(ITNAME))=TNAME 
      MSTRNG(MINTS(IGNAME))=GNAME 
!     derivative parameters                                             
      MOVIES=.FALSE. 
      IF ((GTERM) .AND. (.NOT. TTERM)) MOVIES=.TRUE. 
                                       
      DX=(VXMAX-VXMIN)/NPTS         !spatial step                    
      IMID=NINT((XMID-VXMIN)/DX)    !nearest lattice index to XMID   
      XMID=VXMIN+IMID*DX            !force XMID to be a lattice point
      MREALS(IXMID)=XMID            !change default accordingly      
!     check CYCLES with respect to lattice size                         
      MAXCYC=REAL(NPTS)/4.          !max number of cycles for NPTS   
      IF (CYCLES .GT. MAXCYC) THEN 
         WRITE (OUNIT,*) 'Number of cycles is too large for NPTS' 
         !CYCLES=GETFLT(MAXCYC/10.,-MAXCYC,MAXCYC,                       &
     !&      'Enter number of cycles = K0/PI')                           
         MREALS(ICYCLE)=CYCLES 
      END IF 
      K0=CYCLES*2*PI/(VXMAX-VXMIN) 
      !system('clear')
      CALL POTNTL                                !setup potential array            
      IF ((GTERM) .OR. (GHRDCP)) CALL GRFINT     !initialize graphing param 
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE POTNTL 
! fills the potential array                                             
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'PARAM.E7' 
! Local variables:                                                      
      INTEGER IX 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 5 IX=0,NPTS 
         X(IX)=VXMIN+IX*DX 
    5 END DO 
!Square bump/well                 
      IF (POT .EQ. SQUARE) THEN 
         DO  IX=0,NPTS 
            IF ((X(IX) .GE. (X0-A)) .AND. (X(IX) .LE. (X0+A))) THEN 
               V(IX)=V0 
            ELSE 
               V(IX)=0. 
            END IF 
         END DO 

!Double Square bump/well                 
     ELSE IF (POT .EQ. DOUBLE_SQUARE) THEN 
         DO  IX=0,NPTS 
            IF ((X(IX) .GE. (X0-A)) .AND. (X(IX) .LE. (X0+A))) THEN 
               V(IX)=V0 
            ELSE IF ((X(IX) .GE. ((X0+4*A)-A)) .AND. (X(IX) .LE. ((X0+4*A)+A))) THEN 
               V(IX)=V0 
            ELSE 
               V(IX)=0. 
            END IF 
         END DO 

! GAUSS bump/well         
      ELSE IF (POT .EQ. GAUSS) THEN 
         DO  IX=0,NPTS 
            V(IX)=V0*EXP(-AGAUSS*(X(IX)-X0)**2/A**2) 
           ! V(IX)=V0*EXP(-AGAUSS*(X(IX)-X0)**2/A**2) +  V0*EXP(-AGAUSS*(X(IX)-X0+0.4)**2/A**2) 
         END DO 
!Parabola                         
      ELSE IF (POT .EQ. PARAB) THEN 
         DO  IX=0,NPTS 
            V(IX)=V0*((X(IX)-X0)**2/A**2) 
         END DO 
!Smooth step function             
      ELSE IF (POT .EQ. STEP) THEN 
         DO  IX=0,NPTS 
            V(IX)=V0/2*(2/PI*ATAN(ASTEP*(X(IX)-X0)/A)+1.) 
         END DO 
      END IF 
      VMAX=V(0)      !find VMAX and VMIN to give     
      VMIN=V(0)      !a scale for graphics       
      DO  IX=1,NPTS 
         IF (V(IX) .GT. VMAX) VMAX=V(IX) 
         IF (V(IX) .LT. VMIN) VMIN=V(IX) 
      END DO 
      IF (VMAX .EQ. VMIN) VMAX=VMIN+1     !need a nonzero difference
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE PRMOUT(MUNIT,NLINES) 
! outputs parameter summary to MUNIT                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
       INCLUDE 'IO.ALL' 
       INCLUDE 'PARAM.E7' 
! Passed variables:                                                     
       INTEGER MUNIT            !unit number for output (input)          
       INTEGER NLINES           !number of lines written so far (output)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !IF (MUNIT .EQ. OUNIT) CALL CLEAR 
       WRITE (MUNIT,2) 
       WRITE (MUNIT,4) 
       WRITE (MUNIT,24) XMID,NPTS,DX 
       WRITE (MUNIT,2) 
       IF (POT .EQ. SQUARE) THEN 
         WRITE (MUNIT,6) 
       ELSE IF (POT .EQ. DOUBLE_SQUARE) THEN 
         WRITE (MUNIT,7) 
       ELSE IF (POT .EQ. GAUSS) THEN   
         WRITE (MUNIT,8)               
       ELSE IF (POT .EQ. PARAB) THEN 
         WRITE (MUNIT,10) 
       ELSE IF (POT .EQ. STEP) THEN 
         WRITE (MUNIT,12) 
       END IF 
       WRITE (MUNIT,14) X0,A,V0 
       WRITE (MUNIT,2) 
       IF (PACKET .EQ. LORNTZ) THEN 
         WRITE (MUNIT,16) 
       ELSE IF (PACKET .EQ. GAUSS) THEN 
         WRITE (MUNIT,18) 
       ELSE IF (PACKET .EQ. USER) THEN 
         WRITE (MUNIT,19) 
       END IF 
       WRITE (MUNIT,20) W0,SIGMA 
       WRITE (MUNIT,22) K0,K0**2 
       WRITE (MUNIT,2) 
       NLINES=11 
    2  FORMAT (' ') 
    4  FORMAT                                                           &
     & (' Output from example 7: Time-dependent Schroedinger Equation') 
    6  FORMAT (' Square-barrier/well: V=V0 for X0-A < X < X0+A') 
    7  FORMAT (' Double Square-barrier/well: V=V0 for X0-A < X < X0+A', &
                  ' and   V=V0 for (X0+4*A) -A < X < (X0+4A)+A'     ) 
    8  FORMAT                                                           &
     & (' Gaussian barrier/well: V(X)=V0*(EXP-(AGAUSS*((X-X0)/A)**2))') 
   10  FORMAT (' Parabolic well: V(X)=V0*(X-X0)**2/A**2') 
   12  FORMAT (' Smooth step: V(X)=V0*(2/PI*ATN(ASTEP*(X-X0)/A)+1)/2') 
   14  FORMAT (' X0 = ',1PE12.5,5X,' A = ',1PE12.5,5X,                  &
     &         ' V0 (units K0**2) = ',1PE12.5)                          
   16  FORMAT (' Lorentzian wavepacket:  ',                             &
     &         'PHI(X)=EXP(I*K0*X)/(SIGMA**2+(X-W0)**2)')               
   18  FORMAT (' Gaussian wavepacket: ',                                &
     &         'PHI(X)=EXP(I*K0*X)*EXP(-(X-W0)**2/2/SIGMA**2)')         
   19  FORMAT (' USER defined wavepacket:',                             &
               ' Read from file wavepacket.txt')
   20  FORMAT (' W0 = ',1PE12.5,5X,' SIGMA = ',1PE12.5) 
   22  FORMAT (' K0 = ',1PE12.5,5X,' K0**2 (energy scale) = ',1PE12.5) 
   24  FORMAT (' XMIDDLE = ',1PE12.5,5X,' NPTS = ',I5,5X,               &
     &         ' space step = ',1PE12.5)                                
   26  FORMAT (18X,'energy',9X,'probability',19X,'<x>') 
   28  FORMAT (7X,'time',7X,'K0**2 ',5X,'left',3X,'right',3X,'total',6X,&
     &         'left',4X,'right',4X,'total')                            
   30  FORMAT (7X,'----',7X,'------',5X,'----',3X,'-----',3X,'-----',6X,&
     &         '----',4X,'-----',4X,'-----')                            
       RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE TITLES(MUNIT,NLINES,DT) 
! write out time step and column titles to MUNIT                        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
       INCLUDE 'IO.ALL' 
! Passed variables:                                                     
       INTEGER MUNIT     !unit number for output (input)          
       INTEGER NLINES    !number of lines written so far (output)
       REAL DT           !time step (input)                      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       WRITE (MUNIT,*)  ' ' 
       WRITE (MUNIT,20) DT 
       WRITE (MUNIT,26) 
       WRITE (MUNIT,28) 
       WRITE (MUNIT,30) 
       IF (MUNIT .EQ. OUNIT) NLINES=NLINES+5 
   20  FORMAT (' time step = ',1PE15.8) 
   26  FORMAT (18X,'energy',9X,'probability',19X,'<x>') 
   28  FORMAT (7X,'time',7X,'K0**2 ',5X,'left',3X,'right',3X,'total',6X,&
     &         'left',4X,'right',4X,'total')                            
   30  FORMAT (7X,'----',7X,'------',5X,'----',3X,'-----',3X,'-----',6X,&
     &         '----',4X,'-----',4X,'-----')                            
       RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE TXTOUT(MUNIT,E,TIME,LPROB,RPROB,TPROB,LX,RX,TX,NLINES) 
! writes results for one state to MUNIT                                 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables:                                                     
      INCLUDE 'IO.ALL' 
! Input variables:                                                      
      INTEGER MUNIT           !output unit specifier               
      REAL TIME               !time                                
      REAL TPROB,LPROB,RPROB  !normalization of the wavepacket     
      REAL LX,RX,TX           !average position                    
      REAL E                  !energy                              
      INTEGER NLINES          !number of lines printed so far (I/O)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     if screen is full, clear screen and retype headings               
      IF ((MOD(NLINES,TRMLIN-4) .EQ. 0)       &
     &                          .AND. (MUNIT .EQ. OUNIT)) THEN          
         !CALL PAUSE('to continue...',1) 
         !CALL CLEAR 
         WRITE (MUNIT,26) 
         WRITE (MUNIT,28) 
         WRITE (MUNIT,30) 
         NLINES=NLINES+3 
      END IF 
      WRITE (MUNIT,40) TIME,E,LPROB,RPROB,TPROB,LX,RX,TX 
!     keep track of printed lines only for terminal output              
      IF (MUNIT .EQ. OUNIT) NLINES=NLINES+1 
!                                                                       
   26  FORMAT (18X,'energy',9X,'probability',19X,'<x>') 
   28  FORMAT (7X,'time',7X,'K0**2 ',5X,'left',3X,'right',3X,'total',6X,&
     &         'left',4X,'right',4X,'total')                            
   30  FORMAT (7X,'----',7X,'------',5X,'----',3X,'-----',3X,'-----',6X,&
     &         '----',4X,'-----',4X,'-----')                            
   40  FORMAT (4X,1PE10.3,4X,0PF6.3,4X,F5.3,2(3X,F5.3),                 &
     &  4X,F6.3,2(3X,F6.3))                                             
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE GRFINT 
! initialize all graphing variables which are the same for one run      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables                                                      
      INCLUDE 'PARAM.E7' 
      INCLUDE 'GRFDAT.ALL' 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!find limits on data points        
      YMAX=VMAX 
      YMIN=VMIN 
      X0VAL=VMIN 
      XMIN=VXMIN 
      XMAX=VXMAX 
      Y0VAL=VXMIN 
      
      NPOINT=NPTS+1 
!symbols and ticks                 
      ISYM=1 
      IFREQ=0 
      NXTICK=5 
      NYTICK=5 
      ILINE=1 
!titles and labels                 
      LABEL(1)='scaled X' 
      LABEL(2)='potential, PHI2' 
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SUBROUTINE GRFOUT(DEVICE,PHI2,TIME,TPROB,TX,E)   !  Original
!Redefined by MEK 2019.06.11 
      SUBROUTINE GRFOUT(PHI,PHI2,TIME,TPROB,TX,E)   
! outputs wavepacket and potential to DEVICE                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables                                                      
      INCLUDE 'IO.ALL' 
      INCLUDE 'PARAM.E7' 
      INCLUDE 'GRFDAT.ALL' 
! Input variables:                                                      
                                
      INTEGER DEVICE         !which device is being used?                  
                              
      COMPLEX PHI(0:MAXLAT)  !wavepacket
      REAL PHI2(0:MAXLAT)    !wavepacket squared                                         
      REAL TIME              !time                                   
      REAL TPROB             !normalization of the wavepacket        
      REAL TX                !average position                       
      REAL E                 !energy                                 
! Local variables                                                       
      REAL PHIGRF(0:MAXLAT)  !scaled wavepacket squared               
      REAL VSCALE            !scaling factor for PHI2                
      INTEGER IX             !lattice index                          
      CHARACTER *9 CE,CSIG   !Energy, SIGMA as character data        
      CHARACTER*9 CPROB,CTX,CTIME  !data as characters                    
      INTEGER PLEN,TXLEN,TLEN,ELEN,SLEN   !length of strings               
      INTEGER LENTRU         !true length of character data           
      INTEGER SCREEN         !send to terminal                       
      INTEGER PAPER          !make a hardcopy                        
      INTEGER FILE           !send to a file                         
      DATA SCREEN,PAPER,FILE/1,2,3/ 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     messages for the impatient                                        
      IF (DEVICE .NE. SCREEN) WRITE (OUNIT,100) 
!     calculate parameters for graphing                                 
      !IF (DEVICE .NE. FILE) THEN 
          IF (.NOT. MOVIES) THEN 
            NPLOT=1               !how many plots?              
          ELSE                                                  
            NPLOT=4               !for movies, there are       
          END IF                  ! 4 plots per page           
          IPLOT=1                                               
          !CALL CONVRT(TX,CTX,TXLEN) 
          !CALL CONVRT(TIME,CTIME,TLEN) 
          !INFO='t='//CTIME(1:TLEN)//'  norm='//             &
     !&     CPROB(1:PLEN)//'  <X>='//CTX(1:TXLEN)                        
          !CALL CONVRT(E,CE,ELEN) 
          !CALL CONVRT(SIGMA,CSIG,SLEN) 
          !IF (PACKET .EQ. LORNTZ) THEN 
             !TITLE='Lorentzian packet,' 
          !ELSE IF (PACKET .EQ. GAUSS) THEN 
             !TITLE='Gaussian packet,' 
          !END IF 
          !TITLE=TITLE(1:LENTRU(TITLE))//' width='//CSIG(1:SLEN)   &
     !&       //', E ='//CE(1:ELEN)                                      
!                                                                       
          !CALL GTDEV(DEVICE)                  !device nomination        
           !DEVICE ='FILE'                                               
          !IF (DEVICE .EQ. SCREEN) CALL GMODE  !change to graphics mode 
          !CALL LNLNAX                         !draw axes               
      !END IF 
!     The largest PHI2(IX) can ever become is 1 (all probability at     
!     one lattice point).  The following scale assumes that the maximum 
!     value is 10/NPTS (prob equally shared by one-tenth of the points) 
      VSCALE=(VMAX-VMIN)*NPTS/10.     !scale PHI2 so that it fits on Vscale 
      DO 10 IX=0,NPTS 
         PHIGRF(IX)=VMIN+PHI2(IX)*VSCALE 
         IF(PHIGRF(IX) .GT. VMAX) PHIGRF(IX)=VMAX   !clip high values    
   10 END DO 
!  Writing wavepacket in file                                                                                
       OPEN(66, FILE='wavepacket.txt', FORM='FORMATTED')
!              WRITE (66,'(2(5X,E15.8))')  (REAL(PHI(IX)),CMPLX(PHI(IX)),IX=1,NPTS)
             DO  IX=1,NPTS 
!                WRITE (66,'(2(5X,E15.8))')  REAL(PHI(IX))
                WRITE (66,'(3(5X,E15.8))')  PHI(IX), CABS(PHI(IX))**2
             ENDDO
      CLOSE(66)

!     output results                                                    
      !IF (DEVICE .EQ. FILE) THEN 
         WRITE (GUNIT,60) TIME 
         WRITE (GUNIT,70)  (X(IX),V(IX),PHI2(IX),IX=1,NPTS) 
      !ELSE 
          !CALL XYPLOT (X,V) 
          !CALL XYPLOT (X,PHIGRF) 
      !END IF 
!     end graphing session                                              
      !IF (.NOT. MOVIES) THEN 
        !IF (DEVICE .NE. FILE) CALL GPAGE(DEVICE) 
        !IF (DEVICE .EQ. SCREEN) CALL TMODE 
      !END IF 
   60 FORMAT ('# X,V, PHI2 at time = ',1PE15.8) 
   70 FORMAT (3(5X,E15.8)) 
  100 FORMAT (/,' Patience, please; output going to a file.') 
      RETURN 
      END                                           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE GRFSEC(DEVICE,PHI2,TIME,TPROB,TX,FRAME) 
! outputs wavepacket and potential for movies, frames 2-4               
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Global variables                                                      
      INCLUDE 'IO.ALL' 
      INCLUDE 'PARAM.E7' 
      INCLUDE 'GRFDAT.ALL' 
! Input variables:                                                      
      INTEGER DEVICE              !which device is being used?            
      REAL PHI2(0:MAXLAT)         !wavepacket squared                     
      REAL TIME                   !time                                   
      REAL TPROB                  !normalization of the wavepacket        
      REAL TX                     !average position                       
      INTEGER FRAME               !which frame of the movie is it?        
! Local variables                                                          
      REAL PHIGRF(0:MAXLAT)       !scaled wavepacket squared              
      REAL VSCALE                 !scaling factor for PHI2                
      INTEGER IX                  !lattice index                          
      CHARACTER*9 CPROB,CTX,CTIM    !data as characters                   
      INTEGER PLEN,TXLEN,TLEN     !length of strings                      
      INTEGER SCREEN              !send to terminal                       
      INTEGER PAPER               !make a hardcopy                        
      INTEGER FILE                !send to a file                         
      DATA SCREEN,PAPER,FILE/1,2,3/ 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IPLOT=FRAME                     !associate plots with frames          
      IF (IPLOT .EQ. 0) IPLOT=4                                       
      !CALL CONVRT(TPROB,CPROB,PLEN)  !create legend                       
      !CALL CONVRT(TX,CTX,TXLEN) 
      !CALL CONVRT(TIME,CTIME,TLEN) 
     ! INFO='t='//CTIME(1:TLEN)//'  norm='//                     &
     !&     CPROB(1:PLEN)//'  <X>='//CTX(1:TXLEN)                        
      !CALL LNLNAX 
      VSCALE=(VMAX-VMIN)*NPTS/10. !scale PHI2 so that it fits on Vscale
      DO  IX=0,NPTS 
         PHIGRF(IX)=VMIN+PHI2(IX)*VSCALE 
         IF(PHIGRF(IX) .GT. VMAX) PHIGRF(IX)=VMAX 
      END DO 
      !CALL XYPLOT (X,V) !plot data                           
      !CALL XYPLOT (X,PHIGRF) 
      !IF (IPLOT .EQ. 4) CALL GPAGE(DEVICE) !clear this graphics page   
      RETURN 
      END                                           
