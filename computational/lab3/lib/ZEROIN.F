      REAL FUNCTION ZEROIN(AX,BX,F,TOL)
      REAL AX,BX,F,TOL
C
C     H��� ��HK��� F(X) B���C��ETC� B �HTEPBA�E AX,BX
C
C     BXO�HA� �H�OPMA���..
C
C     AX     �EB�� KOHE� �CXO�HO�O �HTEPBA�A
C     BX     �PAB�� KOHE� �CXO�HO�O �HTEPBA�A
C     F      �O��PO�PAMMA-��HK���, KOTOPA� B���C��ET F(X)
C            ��� ���O�O X B �HTEPBA�E AX BX
C     TOL    �E�AEMA� ���HA �HTEPBA�A HEO�PE�E�EHHOCT�
C            KOHE�HO�O PE����TATA
C
C     B�XO�HA� �H�OPMA���...
C
C     ZEROIN A�C��CCA, A��POKC�M�P���A� H��� ��HK��� F B
C            �HTEPBA�E AX, BX
C
C        �E� �POBEPK� �PE��O�A�AETC�, �TO F(AX) � F(BX) �ME�T
C     �POT�BO�O�O�H�E �HAK�.
C        ZEROIN B���C��ET H��� X B �A�AHHOM �HTEPBA�E AX, BX
C     B �PE�E�AX �O��CKA HA O���K�  4*MACHEPS*ABS(X) + TOL,
C     ��E MACHEPS-OTHOC�TE��HA� MA��HHA� TO�HOCT�.
C        �TA �O��PO�PAMMA-��HK��� �PE�CTAB��ET CO�O� C�E�KA
C     MO������POBAHH�� TPAHC����� A��O� 60-�PO�E��P� ZERO,
C     �P�BE�EHHO� B KH��E RICHARD BRENT, ALGORITHMS FOR
C     MINIMIZATION WITHOUT DERIVATIVES,PRENTICE HALL,INC.(1973).
C
      REAL A,B,C,D,E,EPS,FA,FB,FC,TOL1,XM,P,Q,R,S
C
C     B���C��T� EPS,OTHOC�TE��H�� MA��HH�� TO�HOCT�
C
      EPS=1.0
   10 EPS=EPS/2.0
      TOL1=1.0+EPS
      IF(TOL1.GT.1.0) GO TO 10
C
C     �P�CBOEH�E HA�A��H�X �HA�EH��
C
      A=AX
      B=BX
      FA=F(A)
      FB=F(B)
C
C     HA�AT� �A�
C
   20 C=A
      FC=FA
      D=B-A
      E=D
   30 IF(ABS(FC).GE.ABS(FB)) GO TO 40
      A=B
      B=C
      C=A
      FA=FB
      FB=FC
      FC=FA
C
C     �POBEPKA CXO��MOCT�
C
   40 TOL1=2.0*EPS*ABS(B)+0.5*TOL
      XM=0.5*(C-B)
      IF(ABS(XM).LE.TOL1) GO TO 90
C
C     HEO�XO��MA �� ��CEK���
C
      IF(FB.EQ.0.0) GO TO 90
      IF(ABS(E).LT.TOL1) GO TO 70
      IF(ABS(FA).LE.ABS(FB)) GO TO 70
C
C     BO�MO�HA �� KBA�PAT��HA� �HTEP�O�����
C
      IF(A.NE.C)GO TO 50
C
C     ��HE�HA� �HTEP�O�����
C
      S=FB/FA
      P=2.0*XM*S
      Q=1.0-S
      GO TO 60
C
C     O�PATHA� KBA�PAT��HA� �HTEP�O�����
C
   50 Q=FA/FC
      R=FB/FC
      S=FB/FA
       P=S*(2.0*XM*Q*(Q-R)-(B-A)*(R-1.0))
      Q=(Q-1.0)*(R-1.0)*(S-1.0)
C
C     B��PAT� �HAK�
C
   60 IF(P.GT.0.0) Q=-Q
      P=ABS(P)
C
C     �P�EM�EMA �� �HTEP�O�����
C
      IF((2.0*P).GE.(3.0*XM*Q-ABS(TOL1*Q))) GO TO 70
      IF(P.GE.ABS(0.5*E*Q)) GO TO 70
      E=D
      D=P/Q
      GO TO 80
C
C     ��CEK���
C
   70 D=XM
      E=D
C
C     �ABEP��T� �A�
C
   80 A=B
      FA=FB
      IF(ABS(D).GT.TOL1) B=B+D
      IF(ABS(D).LE.TOL1) B=B+SIGN(TOL1,XM)
      FB=F(B)
      IF((FB*(FC/ABS(FC))).GT.0.0) GO TO 20
      GO TO 30
C
C     KOH�EHO
C
   90 ZEROIN=B
      RETURN
      END
