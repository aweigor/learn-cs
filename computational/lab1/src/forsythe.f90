module Forsythe
   
   implicit none
   
contains
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
   
end module Forsythe