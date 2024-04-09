      SUBROUTINE QUANC8(FUN,A,B,ABSERR,RELERR,RESULT,
     *                  ERREST,NOFUN,FLAG)
C
      REAL FUN,A,B,ABSERR,RELERR,RESULT,ERREST,FLAG
      INTEGER NOFUN
C
C     O˜EH˜T˜ ˜HTE˜PA˜ ˜˜˜ FUN(X) OT A ˜O B C ˜A˜AHHO˜
C     ˜O˜˜˜OBATE˜EM TO˜HOCT˜˜
C     ABTOMAT˜˜ECKA˜ A˜A˜T˜BHA˜ ˜PO˜PAMMA, OCHOBAHHA˜ HA
C     ˜OPM˜˜E H˜˜TOHA-KOTE˜A 8-O˜O ˜OP˜˜KA
C
C     BXO˜HA˜ ˜H˜OPMA˜˜˜
C
C    FUN    -˜M˜ BHE˜HE˜ ˜˜HK˜˜˜,PEA˜˜˜˜˜˜E˜
C            B˜˜˜C˜EH˜E ˜O˜˜HTE˜PA˜˜HO˜ ˜˜HK˜˜˜
C
C    A      -H˜˜H˜˜  ˜PE˜E˜ ˜HTE˜P˜POBAH˜˜
C
C    B      -BEPXH˜˜ ˜PE˜E˜ ˜HTE˜P˜POBAH˜˜ (B MO˜ET
C            ˜˜T˜ MEH˜˜E, ˜EM  A)
C
C    RELERR -OTHOC˜TE˜˜HA˜ ˜E˜AEMA˜ ˜O˜PE˜HOCT˜
C
C    ABSERR -A˜CO˜˜THA˜ ˜E˜AEMA˜  ˜O˜PE˜HOCT˜
C
C    B˜XO˜HA˜ ˜H˜OPMA˜˜˜
C
C    RESULT -˜P˜˜˜˜˜EHHOE ˜HA˜EH˜E ˜HTE˜PA˜A
C
C    ERREST -O˜EHKA BE˜˜˜˜H˜ ˜E˜CTB˜TE˜˜HO˜ O˜˜˜K˜
C
C    NOFUN  -˜˜C˜O ˜HA˜EH˜˜ ˜˜HK˜˜˜,˜C˜O˜˜˜OBAHH˜X
C            ˜P˜ B˜˜˜C˜EH˜˜ ˜HTE˜PA˜A.
C
C    FLAG   -˜H˜˜KATOP HA˜E˜HOCT˜. EC˜˜ FLAG PABEH H˜˜˜,
C            TO RESULT ˜˜OB˜ETBOP˜ET ˜A˜AHHO˜ ˜PAH˜˜E
C            ˜O˜PE˜HOCT˜. EC˜˜ FLAG=XXX.YYY, TO  XXX-˜˜C˜O
C            ˜HTEPBA˜OB ˜˜˜ KOTOP˜X HE ˜˜˜O CXO˜˜MOCT˜,
C            A 0.YYY-˜O˜˜ OCHOBHO˜O ˜HTEPBA˜A, OCTAB˜A˜C˜
C            ˜˜˜ O˜PA˜OTK˜ B TOT MOMEHT,KO˜˜A ˜O˜˜PO˜PAMMA
C            ˜P˜˜˜˜˜˜˜AC˜ K ˜PE˜E˜˜HOM˜ ˜˜C˜˜ ˜˜˜ NOFUN.
C
      REAL W0,W1,W2,W3,W4,AREA,X0,F0,STONE,STEP,COR11,TEMP
      REAL QPREV,QNOW,QDIFF,QLEFT,ESTERR,TOLERR
      REAL QRIGHT(31),F(16),X(16),FSAVE(8,30),XSAVE(8,30)
C
C     *** ˜TA˜1 *** ˜P˜CBOEH˜E HA˜A˜˜H˜X ˜HA˜EH˜˜
C     ˜EPEMEHH˜M, HE ˜AB˜C˜˜˜M OT ˜HTEPBA˜A.
C     ˜EHEP˜POBAH˜E KOHCTAHT.
C
      INTEGER LEVMIN,LEVMAX,LEVOUT,NOMAX,NOFIN,LEV,NIM,I,J
      LEVMIN=1
      LEVMAX=30
      LEVOUT=6
      NOMAX=5000
      NOFIN=NOMAX-8*(LEVMAX-LEVOUT+2**(LEVOUT+1))
C
C     EC˜˜ NOFUN ˜OCT˜˜AET ˜HA˜EH˜˜ NOFIN, TO TPEBO˜A
C
      W0=3956.0/14175.0
      W1=23552.0/14175.0
      W2=-3712.0/14175.0
      W3=41984.0/14175.0
      W4=-18160.0/14175.0
C
C     ˜P˜CBO˜T˜ H˜˜EB˜E ˜HA˜EH˜˜ ˜EPEMEHH˜M C˜MMAM
C
      FLAG=0.0
      RESULT=0.0
      COR11=0.0
      ERREST=0.0
      AREA=0.0
      NOFUN=0
      IF(A.EQ.B)RETURN
C
C     *** ˜TA˜ 2 *** ˜P˜CBOEH˜E HA˜A˜˜H˜X ˜HA˜EH˜˜
C     ˜EPEMEHH˜M, ˜AB˜C˜˜˜M OT ˜HTEPBA˜A, B
C     COOTBETCTB˜˜ C ˜EPB˜M ˜HTEPBA˜OM
C
      LEV=0
      NIM=1
      X0=A
      X(16)=B
      QPREV=0.0
      F0=FUN(X0)
      STONE=(B-A)/16.0
      X(8)=(X0+X(16))/2.0
      X(4)=(X0+X(8))/2.0
      X(12)=(X(8)+X(16))/2.0
      X(2)=(X0+X(4))/2.0
      X(6)=(X(4)+X(8))/2.0
      X(10)=(X(8)+X(12))/2.0
      X(14)=(X(12)+X(16))/2.0
      DO 25 J=2,16,2
      F(J)=FUN(X(J))
   25 CONTINUE
      NOFUN=9
C
C     *** ˜TA˜ 3 *** OCHOBH˜E B˜˜˜C˜EH˜˜
C     TPE˜˜˜TC˜ QPREV, X0, X2, X4,...,X16, F0, F2, F4,...
C     ,F16. B˜˜˜C˜˜˜TC˜ X1, X3,...,X15, F1, F3,...,F15,
C     QLEFT,QRIGHT,QNOW,QDIF,AREA
C
   30 X(1)=(X0+X(2))/2.0
      F(1)=FUN(X(1))
      DO 35 J=3,15,2
      X(J)=(X(J-1)+X(J+1))/2.0
      F(J)=FUN(X(J))
   35 CONTINUE
      NOFUN=NOFUN+8
      STEP=(X(16)-X0)/16.0
      QLEFT=(W0*(F0+F(8))+W1*(F(1)+F(7))+W2*
     *      (F(2)+F(6))+W3*(F(3)+F(5))+W4*F(4))*STEP
      QRIGHT(LEV+1)=(W0*(F(8)+F(16))+W1*(F(9)+F(15))+
     *       W2*(F(10)+F(14))+W3*(F(11)+F(13))+W4*F(12))*STEP
      QNOW=QLEFT+QRIGHT(LEV+1)
      QDIFF=QNOW-QPREV
      AREA=AREA+QDIFF
C
C     *** ˜TA˜ 4 *** ˜POBEPKA CXO˜˜MOCT˜ ˜˜˜ ˜HTEPBA˜A
C
      ESTERR=ABS(QDIFF)/1023.0
      TOLERR=AMAX1(ABSERR,RELERR*ABS(AREA))*(STEP/STONE)
      IF(LEV.LT.LEVMIN)GO TO 50
      IF(LEV.GE.LEVMAX)GO TO 62
      IF(NOFUN.GT.NOFIN)GO TO 60
      IF(ESTERR.LE.TOLERR)GO TO 70
C
C     *** ˜TA˜ 5 *** CXO˜˜MOCT˜ HET.
C     ˜CTAHOB˜T˜ C˜E˜˜˜˜˜˜ ˜HTEPBA˜
C
   50 NIM=2*NIM
      LEV=LEV+1
C
C     ˜A˜OMH˜T˜ ˜˜EMEHT˜, OTHOC˜˜˜EC˜ K ˜PABO˜ ˜O˜OB˜HE
C     ˜HTEPBA˜A, ˜˜˜ ˜˜˜˜˜E˜O ˜C˜O˜˜˜OBAH˜˜.
C
      DO 52 I=1,8
      FSAVE(I,LEV)=F(I+8)
      XSAVE(I,LEV)=X(I+8)
   52 CONTINUE
C
C     CO˜PAT˜ ˜˜EMEHT˜, OTHOC˜˜˜EC˜ K ˜EBO˜ ˜O˜OB˜HE
C     ˜HTEPBA˜A ˜˜˜ HEME˜˜EHHO˜O ˜C˜O˜˜˜OBAH˜˜
C
      QPREV=QLEFT
      DO 55 I=1,8
      J=-I
      F(2*J+18)=F(J+9)
      X(2*J+18)=X(J+9)
   55 CONTINUE
      GO TO 30
C
C     *** ˜TA˜ 6 *** "˜O˜APH˜˜" PA˜˜E˜
C     ˜˜C˜O ˜HA˜EH˜˜ ˜˜HK˜˜˜ ˜˜˜˜KO K TOM˜, ˜TO˜˜
C     ˜PEB˜C˜T˜ ˜CTAHOB˜EHH˜˜ ˜PE˜E˜.
C
   60 NOFIN=2*NOFIN
      LEVMAX=LEVOUT
      FLAG=FLAG+(B-X0)/(B-A)
      GO TO 70
C
C     TEK˜˜EE ˜PE˜E˜˜HOE ˜HA˜EH˜E ˜˜˜˜˜H˜ ˜E˜EH˜˜
C     ˜O˜O˜AM PABHO LEVMAX
C
   62 FLAG=FLAG+1.0
C
C     *** ˜TA˜ 7 *** CXO˜˜MOCT˜ ˜˜˜ ˜HTEPBA˜A ˜MEET MECTO
C     ˜P˜˜AB˜T˜ O˜EPE˜H˜E C˜A˜AEM˜E K ˜EPEMEHH˜M C˜MMAM.
C
   70 RESULT=RESULT+QNOW
      ERREST=ERREST+ESTERR
      COR11=COR11+QDIFF/1023.0
C
C     ˜CTAHOB˜T˜ C˜E˜˜˜˜˜˜ ˜HTEPBA˜.
C
   72 IF(NIM.EQ.2*(NIM/2))GO TO 75
      NIM=NIM/2
      LEV=LEV-1
      GO TO 72
   75 NIM=NIM+1
      IF(LEV.LE.0)GO TO 80
C
C     CO˜PAT˜ ˜˜EMEHT˜, HEO˜XO˜˜M˜E ˜˜˜ C˜E˜˜˜˜E˜O ˜HTEBA˜A
C
      QPREV=QRIGHT(LEV)
      X0=X(16)
      F0=F(16)
      DO 78 I=1,8
      F(2*I)=FSAVE(I,LEV)
      X(2*I)=XSAVE(I,LEV)
   78 CONTINUE
      GO TO 30
C
C     *** ˜TA˜ 8 *** ˜AK˜˜˜˜TE˜˜H˜E O˜EPA˜˜˜ ˜ B˜XO˜
C
   80 RESULT=RESULT+COR11
C
C     O˜EC˜E˜˜T˜, ˜TO˜˜ ˜HA˜EH˜E ˜EPEMEHHO˜ ERREST
C     ˜˜˜O HE MEH˜˜E ˜POBH˜ OKP˜˜˜EH˜˜
C
      IF(ERREST.EQ.0.0)RETURN
   82 TEMP=ABS(RESULT)+ERREST
      IF(TEMP.NE.ABS(RESULT))RETURN
      ERREST=2.0*ERREST
      GO TO 82
      END
