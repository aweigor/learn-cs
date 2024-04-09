module Forsythe
   
   implicit none
   
contains
   SUBROUTINE RKF45(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
      EXTERNAL F
      INTEGER NEQN,IFLAG,IWORK(5)
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,WORK(1)

      INTEGER K1,K2,K3,K4,K5,K6,K1M

      K1M=NEQN+1
      K1=K1M+1
      K2=K1+NEQN
      K3=K2+NEQN
      K4=K3+NEQN
      K5=K4+NEQN
      K6=K5+NEQN

      CALL RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK(1),&
         WORK(K1M),WORK(K1),WORK(K2),WORK(K3),WORK(K4),WORK(K5),&
         WORK(K6),WORK(K6+1),IWORK(1),IWORK(2),IWORK(3),IWORK(4),IWORK(5))
      
      RETURN
   END

   SUBROUTINE SOLVE(NDIM,N,A,B,IPVT)
   
         INTEGER NDIM,N,IPVT(N)
         REAL A(NDIM,N),B(N)
         
         INTEGER KB,KM1,NM1,KP1,I,K,M
         REAL T
         
         IF(N.EQ.1) GO TO 50
         NM1=N-1
         DO 20 K=1,NM1
           KP1=K+1
           M=IPVT(K)
           T=B(M)
           B(M)=B(K)
           B(K)=T
           DO 10 I=KP1,N
             B(I)=B(I)+A(I,K)*T
      10   CONTINUE
      20 CONTINUE
      
         DO 40 KB=1,NM1
           KM1=N-KB
           K=KM1+1
           B(K)=B(K)/A(K,K)
           T=-B(K)
           DO 30 I=1,KM1
             B(I)=B(I)+A(I,K)*T
      30   CONTINUE
      40 CONTINUE
      50 B(1)=B(1)/A(1,1)
         RETURN
    END


   SUBROUTINE DECOMP(NDIM,N,A,COND,IPVT,WORK)

   INTEGER NDIM,N
   REAL A(NDIM,N),COND,WORK(N)
   INTEGER IPVT(N)

   REAL EK,T,ANORM,YNORM,ZNORM
   INTEGER NM1,I,J,K,KP1,KB,KM1,M

  IPVT(N)=1
  IF(N.EQ.1)GO TO 80
  NM1=N-1

  ANORM=0.0
  DO 10 J=1,N
   T=0.0
                  DO 5 I=1,N
                     T=T+ABS(A(I,J))
            5   CONTINUE
                  IF(T.GT.ANORM) ANORM=T
          10 CONTINUE

               DO 35 K=1,NM1
                  KP1=K+1

                  M=K
                  DO 15 I=KP1,N
                     IF(ABS(A(I,K)).GT.ABS(A(M,K))) M=I
          15   CONTINUE
                  IPVT(K)=M
                  IF(M.NE.K)IPVT(N)=-IPVT(N)
                  T=A(M,K)
                  A(M,K)=A(K,K)
                  A(K,K)=T

                  IF(T.EQ.0.0)GO TO 35

                  DO 20 I=KP1,N
                     A(I,K)=-A(I,K)/T
          20   CONTINUE

                  DO 30 J=KP1,N
                     T=A(M,J)
                     A(M,J)=A(K,J)
                     A(K,J)=T
                     IF(T.EQ.0.0)GO TO 30
                     DO 25 I=KP1,N
                        A(I,J)=A(I,J)+A(I,K)*T
          25     CONTINUE
          30   CONTINUE
          35 CONTINUE

               DO 50 K=1,N
                  T=0.0
                  IF(K.EQ.1)GO TO 45
                  KM1=K-1
                  DO 40 I=1,KM1
                     T=T+A(I,K)*WORK(I)
          40   CONTINUE
          45   EK=1.0
                  IF(T.LT.0.0)EK=-1.0
                  IF(A(K,K).EQ.0.0)GO TO 90
                  WORK(K)=-(EK+T)/A(K,K)
          50 CONTINUE
               DO 60 KB=1,NM1
                  K=N-KB
                  T=WORK(K)
                  KP1=K+1
                  DO 55 I=KP1,N
                     T=T+A(I,K)*WORK(I)
          55   CONTINUE
                  WORK(K)=T
                  M=IPVT(K)
                  IF(M.EQ.K)GO TO 60
                  T=WORK(M)
                  WORK(M)=WORK(K)
                  WORK(K)=T
          60 CONTINUE

               YNORM=0.0
               DO 65 I=1,N
                  YNORM=YNORM+ABS(WORK(I))
          65 CONTINUE

               CALL SOLVE(NDIM,N,A,WORK,IPVT)

               ZNORM=0.0
               DO 70 I=1,N
                  ZNORM=ZNORM+ABS(WORK(I))
          70 CONTINUE

               COND=ANORM*ZNORM/YNORM
               IF(COND.LT.1.0)COND=1.0
               RETURN

          80 COND=1.0
               IF(A(1,1).NE.0.0)RETURN

          90 CONTINUE
               COND=1.0E+32
               RETURN
               END

   SUBROUTINE SPLINE(N,X,Y,B,C,D)
                  INTEGER N
                  REAL X(N),Y(N),B(N),C(N),D(N)
           
                  INTEGER NM1,IB,I
                  REAL T
           
                  NM1=N-1
                  IF(N.LT.2) RETURN
                  IF(N.LT.3) GO TO 50
           
                  D(1)=X(2)-X(1)
                  C(2)=(Y(2)-Y(1))/D(1)
                  DO 10 I=2,NM1
                     D(I)=X(I+1)-X(I)
                     B(I)=2.*(D(I-1)+D(I))
                     C(I+1)=(Y(I+1)-Y(I))/D(I)
                     C(I)=C(I+1)-C(I)
           10     CONTINUE
           
                  B(1)=-D(1)
                  B(N)=-D(N-1)
                  C(1)=0.
                  C(N)=0.
                  IF(N.EQ.3) GO TO 15
                  C(1)=C(3)/(X(4)-X(2))-C(2)/(X(3)-X(1))
                  C(N)=C(N-1)/(X(N)-X(N-2))-C(N-2)/(X(N-1)-X(N-3))
                  C(1)=C(1)*D(1)**2/(X(4)-X(1))
                  C(N)=-C(N)*D(N-1)**2/(X(N)-X(N-3))
           
           15     DO 20 I=2,N
                     T=D(I-1)/B(I-1)
                     B(I)=B(I)-T*D(I-1)
                     C(I)=C(I)-T*C(I-1)
           20     CONTINUE
           
                  C(N)=C(N)/B(N)
                  DO 30 IB=1,NM1
                     I=N-IB
                     C(I)=(C(I)-D(I)*C(I+1))/B(I)
           30     CONTINUE
            
                  B(N)=(Y(N)-Y(NM1))/D(NM1)+D(NM1)*(C(NM1)+2.*C(N))
                  DO 40 I=1,NM1
                     B(I)=(Y(I+1)-Y(I))/D(I)-D(I)*(C(I+1)+2.*C(I))
                     D(I)=(C(I+1)-C(I))/D(I)
                     C(I)=3.*C(I)
           40     CONTINUE
                   C(N)=3.*C(N)
                   D(N)=D(N-1)
                   RETURN
                   
           50     B(1)=(Y(2)-Y(1))/(X(2)-X(1))
                  C(1)=0.
                  D(1)=0.
                  B(2)=B(1)
                  C(2)=0.
                  D(2)=0.
                  RETURN
   END

   REAL FUNCTION SEVAL(N,U,X,Y,B,C,D)
      INTEGER N
      REAL U,X(N),Y(N),B(N),C(N),D(N)
      
      INTEGER I,J,K
      REAL DX
      DATA I/1/
      IF(I.GE.N) I=1
      IF(U.LT.X(I)) GO TO 10
      IF(U.LE.X(I+1)) GO TO 30
      
 10   I=1
      J=N+1
 20   K=(I+J)/2
      IF(U.LT.X(K))J=K
      IF(U.GE.X(K))I=K
      IF(J.GT.I+1)GO TO 20
      
 30   DX=U-X(I)
      SEVAL=Y(I)+DX*(B(I)+DX*(C(I)+DX*D(I)))
      RETURN
   END

   SUBROUTINE FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,S)
      EXTERNAL F
      INTEGER NEQN
      REAL Y(NEQN),YP(NEQN),F1(NEQN),F2(NEQN),F3(NEQN),F4(NEQN),F5(NEQN),S(NEQN)
      REAL CH, T, H
      INTEGER K
      CH=H/4.0
      DO 221 K=1,NEQN
      F5(K)=Y(K)+CH*YP(K)
  221 CONTINUE 
      CALL F(T+CH,F5,F1)
      CH=3.0*H/32.0
      DO 222 K=1,NEQN
      F5(K)=Y(K)+CH*(YP(K)+3.0*F1(K))
  222 CONTINUE
      CALL F(T+3.0*H/8.0,F5,F2)
      CH=H/2197.0
      DO 223 K=1,NEQN
      F5(K)=Y(K)+CH*(1932.0*YP(K)+(7296.0*F2(K)-7200.0*F1(K)))
  223 CONTINUE
      CALL F(T+12.0*H/13.0,F5,F3)
      CH=H/4104.0
      DO 224 K=1,NEQN
      F5(K)=Y(K)+CH*((8341.0*YP(K)-845.0*F3(K))+(29440.0*F2(K)-32832.0*F1(K)))
  224 CONTINUE
      CALL F(T+H,F5,F4)
      CH=H/20520.0
      DO 225 K=1,NEQN
      F1(K)=Y(K)+CH*((-6080.0*YP(K)+(9295.0*F3(K)-5643.0*F4(K)))+(41040.0*F1(K)-28352.0*F2(K)))
  225 CONTINUE
      CALL F(T+H/2.0,F1,F5)
      CH=H/7618050.0
      DO 230 K=1,NEQN
      S(K)=Y(K)+CH*((902880.0*YP(K)+(3855735.0*F3(K)-1371249.0*F4(K)))+(3953664.0*F2(K)+277020.0*F5(K)))
  230 CONTINUE
   RETURN
   END

   SUBROUTINE RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,YP,H,&
                   F1,F2,F3,F4,F5,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG,KFLAG)
     
      LOGICAL HFAILD,OUTPUT
      
      INTEGER NEQN,IFLAG,NFE,KOP,INIT,JFLAG,KFLAG
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,H,YP(NEQN),F1(NEQN), &
           F2(NEQN),F3(NEQN),F4(NEQN),F5(NEQN), SAVRE,SAVAE
     
      EXTERNAL F
      
      REAL A,AE,DT,EE,EEOET,ESTTOL,ET,HMIN,REMIN,RER,S,SCALE,TOL,TOLN,U26,EPSP1,EPS,YPK
     
      INTEGER K,MAXNFE,MFLAG
      
      REAL AMAX1,AMIN1
      
      !SAVE ALL
      DATA REMIN/1.E-12/
      
      DATA MAXNFE/3000/
      
      IF(NEQN.LT.1)GO TO 10
      IF((RELERR.LT.0.0).OR.(ABSERR.LT.0.0))GO TO 10
      MFLAG=IABS(IFLAG)
      IF((MFLAG.EQ.0).OR.(MFLAG.GT.8))GO TO 10
      IF(MFLAG.NE.1)GO TO 20
      
      EPS=1.0
    5 EPS=EPS/2.0
      EPSP1=EPS+1.
      IF(EPSP1.GT.1.)GO TO 5
      U26=26.*EPS
      GO TO 50
      
   10 IFLAG=8
      RETURN
      
   20 IF((T.EQ.TOUT).AND.(KFLAG.NE.3))GO TO 10
      IF(MFLAG.NE.2)GO TO 25
      
      IF((KFLAG.EQ.3).OR.(INIT.EQ.0))GO TO 45
      IF(KFLAG.EQ.4)GO TO 40
      IF((KFLAG.EQ.5).AND.(ABSERR.EQ.0.0))GO TO 30
      IF((KFLAG.EQ.6).AND.(RELERR.LE.SAVRE).AND.(ABSERR.LE.SAVAE))GO TO 30
      GO TO 50
      
   25 IF(IFLAG.EQ.3)GO TO 45
      IF(IFLAG.EQ.4)GO TO 40
      IF((IFLAG.EQ.5).AND.(ABSERR.GT.0.0))GO TO 45
      
   30 STOP
      
   40 NFE=0
      IF(MFLAG.EQ.2)GO TO 50
      
   45 IFLAG=JFLAG
      IF(KFLAG.EQ.3)MFLAG=IABS(IFLAG)
      
   50 JFLAG=IFLAG
      KFLAG=0
      
      SAVRE=RELERR
      SAVAE=ABSERR
      
      RER=2.*EPS+REMIN
      IF(RELERR.GE.RER)GO TO 55
      
      RELERR=RER
      IFLAG=3
      KFLAG=3
      RETURN
      
   55 DT=TOUT-T
   
      IF(MFLAG.EQ.1)GO TO 60
      IF(INIT.EQ.0)GO TO 65
      GO TO 80
      
   60 INIT=0
      KOP=0
      
      A=T
      CALL F(A,Y,YP)
      NFE=1
      IF(T.NE.TOUT)GO TO 65
      IFLAG=2
      RETURN
      
   65 INIT=1
      H=ABS(DT)
      TOLN=0.
      DO 70 K=1,NEQN
      TOL=RELERR*ABS(Y(K))+ABSERR
      IF(TOL.LE.0)GO TO 70
      TOLN=TOL
      YPK=ABS(YP(K))
      IF(YPK*H**5.GT.TOL)H=(TOL/YPK)**0.2
   70 CONTINUE
      IF(TOLN.LE.0.0)H=0.0
      H=AMAX1(H,U26*AMAX1(ABS(T),ABS(DT)))
      JFLAG=ISIGN(2,IFLAG)
      
   80 H=SIGN(H,DT)
   
      IF(ABS(H).GE.2.0*ABS(DT))KOP=KOP+1
      IF(KOP.NE.100)GO TO 85
      
      KOP=0
      IFLAG=7
      RETURN
      
   85 IF(ABS(DT).GT.U26*ABS(T))GO TO 95
   
      DO 90 K=1,NEQN
      Y(K)=Y(K)+DT*YP(K)
   90 CONTINUE
      A=TOUT
      CALL F(A,Y,YP)
      NFE=NFE+1
      GO TO 300
      
   95 OUTPUT=.FALSE.
   
      SCALE=2./RELERR
      AE=SCALE*ABSERR
      
  100 HFAILD=.FALSE.
  
      HMIN=U26*ABS(T)
      
      DT=TOUT-T
      IF(ABS(DT).GE.2.*ABS(H))GO TO 200
      IF(ABS(DT).GT.ABS(H))GO TO 150
      
      OUTPUT=.TRUE.
      H=DT
      GO TO 200
      
  150 H=0.5*DT
  
  200 IF(NFE.LE.MAXNFE)GO TO 220
  
      IFLAG=4
      KFLAG=4
      RETURN
      
  220 CALL FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,F1)
      NFE=NFE+5
      
      EEOET=0.
      DO 250 K=1,NEQN
      ET=ABS(Y(K))+ABS(F1(K))+AE
      IF(ET.GT.0.)GO TO 240
      
      IFLAG=5
      KFLAG=5
      RETURN
      
  240 EE=ABS((-2090.*YP(K)+(21970.*F3(K)-15048.*F4(K)))+(22528.*F2(K)-27360.*F5(K)))
      EEOET=AMAX1(EEOET,EE/ET)
  250 CONTINUE
  
      ESTTOL=ABS(H)*EEOET*SCALE/752400.
      
      IF(ESTTOL.LE.1.0)GO TO 260
      
      HFAILD=.TRUE.
      OUTPUT=.FALSE.
      S=0.1
      IF(ESTTOL.LT.59049.)S=0.9/ESTTOL**0.2
      H=S*H
      IF(ABS(H).GT.HMIN)GO TO 200
      
      IFLAG=6
      KFLAG=6
      RETURN
      
  260 T=T+H
      DO 270 K=1,NEQN
      Y(K)=F1(K)
  270 CONTINUE
      A=T
      CALL F(A,Y,YP)
      NFE=NFE+1
      
      S=5.
      IF(ESTTOL.GT.1.889568E-4)S=0.9/ESTTOL**0.2
      IF(HFAILD)S=AMIN1(S,1.0)
      H=SIGN(AMAX1(S*ABS(H),HMIN),H)
      
      IF(OUTPUT)GO TO 300
      IF(IFLAG.GT.0)GO TO 100
      
      IFLAG=-2
      RETURN
      
  300 T=TOUT
      IFLAG=2
      RETURN

   END

  
   
end module Forsythe