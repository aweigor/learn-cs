      SUBROUTINE RKF45(F,NEQN,Y,T,TOUT,RELERR,ABSERR,
     *                 IFLAG,WORK,IWORK)
C
C     METO˜ P˜H˜E-K˜TTA ˜E˜˜˜EP˜A ˜ETBEPTO˜O-˜˜TO˜O ˜OP˜˜KA
C
C     COCTAB˜TE˜˜ ˜PO˜PAMM˜-H.A.WATTS,L.F.SHAMPINE
C                SANDIA LABORATORIES
C              ALBUQUERQUE, NEW MEXICO
C
C     RKF45 ˜PE˜HA˜HA˜EHA ˜˜ABH˜M O˜PA˜OM ˜˜˜ PE˜EH˜˜
C     HE˜ECTK˜X ˜ C˜A˜O ˜ECTK˜X ˜˜˜˜EPEH˜˜A˜˜H˜X ˜PABHEH˜˜,
C     KO˜˜A B˜˜˜C˜EH˜E ˜PO˜˜BO˜H˜X HE C˜˜˜KOM ˜OPO˜OCTO˜˜EE.
C     RKF45, BOO˜˜E ˜OBOP˜,HE C˜E˜˜ET ˜C˜O˜˜˜OBAT˜
C     EC˜˜ ˜O˜˜˜OBATE˜˜ TPE˜˜ETC˜ B˜COKA˜ TO˜HOCT˜
C
C     PE˜˜ME
C
C     ˜O˜˜PO˜PAMMA RKF45 ˜HTE˜P˜P˜ET C˜CTEM˜ ˜˜ NEQN
C     O˜˜KHOBEHH˜X ˜˜˜˜EPEH˜˜A˜˜H˜X ˜PABHEH˜˜ ˜EPBO˜O
C     ˜OP˜˜KA C˜E˜˜˜˜E˜O B˜˜A:
C
C            DY(I)/DT=F(T,Y(1),Y(2),...,Y(NEQN),
C
C     ˜˜E Y(I) ˜A˜AH˜ B T.
C     O˜˜˜HO ˜O˜˜PO˜PAMM˜ ˜P˜MEH˜˜T ˜˜˜ ˜HTE˜P˜POBAH˜˜
C     OT T ˜O TOUT, O˜HAKO EE MO˜HO ˜C˜O˜˜˜OBAT˜ ˜ KAK
C     O˜HO˜A˜OB˜˜ ˜HTE˜PATOP,˜TO˜˜ ˜PO˜O˜˜˜T˜ PE˜EH˜E HA
C     O˜˜H ˜A˜ B HA˜PAB˜EH˜˜ TOUT.HA B˜XO˜E ˜APAMETPAM,
C     ˜˜˜˜P˜P˜˜˜˜M B C˜˜CKE B˜˜OBA, ˜P˜CBA˜BA˜TC˜ ˜HA˜EH˜˜,
C     HEO˜XO˜˜M˜E ˜˜˜ ˜PO˜O˜˜EH˜˜ ˜HTE˜P˜POBAH˜˜. ˜O˜˜˜O-
C     BATE˜˜ H˜˜HO ˜˜˜˜ E˜E PA˜ O˜PAT˜T˜C˜ K RKF45
C     (˜,BO˜MO˜HO,O˜PE˜E˜˜T˜ HOBOE ˜HA˜EH˜E ˜˜˜ TOUT).
C     B ˜E˜CTB˜TE˜˜HOCT˜ RKF45-˜TO ˜PO˜PAMMA ˜HTEP˜E˜CA,
C     KOTOPA˜ B˜˜˜BAET ˜O˜˜PO˜PAMM˜ RKFS, OC˜˜ECTB˜˜˜˜˜˜
C     ˜PO˜ECC PE˜EH˜˜.RKFS B CBO˜ O˜EPE˜˜ B˜˜˜BAET ˜O˜˜PO˜-
C     PAMM˜ FEHL, KOTOPA˜ B˜˜˜C˜˜ET ˜P˜˜˜˜˜EHHOE PE˜EH˜E
C     HA O˜˜H ˜A˜.
C
C     RKF45 ˜C˜O˜˜˜˜ET METO˜ P˜H˜E-K˜TTA-˜E˜˜˜EP˜A, O˜˜CAHH˜˜
C     B C˜E˜˜˜˜E˜ ˜˜˜˜˜KA˜˜˜:E.FEHLBERG,LOW-ORDER CLASSICAL
C     RUNGE-KUTTA FORMULAS WITH STEPSIZE CONTROL,NASA TR R-315
C
C     CT˜˜˜ PA˜OT˜ ˜PO˜PAMM˜ RKF45 ˜˜˜˜CTP˜P˜ETC˜ B C˜E˜˜˜˜˜X
C     ˜˜˜˜˜KA˜˜˜X:L.F.SHAMPINE,H.A.WATTS,S.DAVENPORT, SOLVING
C     NON-STIFF ORDINARY DIFFERENTIAL EQUATIONS-THE STAT OF
C     THE ART,SANDIA LABORATORIES REPORT SAND75-0182,SIAM
C     REVIEW,18(1976), N3,376-411.
C
C     ˜APAMETP˜ ˜PO˜PAMM˜:
C
C     F       -˜O˜˜PO˜PAMMA F(T,Y,YP) ˜˜˜ B˜˜˜C˜EH˜˜
C              ˜PO˜˜BO˜H˜X YP(I)=DY(I)/DT
C     NEQN    -˜˜C˜O ˜HTE˜P˜P˜EM˜X ˜PABHEH˜˜
C     Y(*)    -PE˜EH˜E B TO˜KE T
C     T       -HE˜AB˜C˜MA˜ ˜EPEMEHHA˜
C     TOUT    -TO˜KA B˜XO˜A,B KOTOPO˜ H˜˜HO O˜PE˜E˜˜T˜
C              ˜HA˜EH˜E PE˜EH˜˜
C     RELERR  -˜PAH˜˜A OTHOC˜TE˜˜HO˜ ˜O˜PE˜HOCT˜
C              ˜˜˜ TECTA ˜OKA˜˜HO˜ O˜˜˜K˜.
C     ABSERR  -˜PAH˜˜A ABCO˜˜THO˜ ˜O˜PE˜HOCT˜
C              ˜˜˜ TECTA ˜OKA˜˜HO˜ O˜˜˜K˜.
C              HA KA˜˜OM ˜A˜E ˜PO˜PAMMA TPE˜˜ET B˜˜O˜HEH˜˜ ˜C˜OB˜˜
C              ABS(LOCAL ERROR).LE.RELERR*ABS(Y)+ABSERR
C              ˜˜˜ KA˜˜O˜ KOM˜OHEHT˜ BEKTOPOB ˜OKA˜˜HO˜
C              O˜˜˜K˜ ˜ PE˜EH˜˜
C     IFLAG   -˜KA˜ATE˜˜ PE˜˜MA ˜HTE˜P˜POBAH˜˜.
C     WORK(*) -MACC˜B,CO˜EP˜A˜˜˜ ˜H˜OPMA˜˜˜,BH˜TPEHH˜˜ ˜˜˜ RKF45,
C              KOTOPA˜ HEO˜XO˜˜MA ˜P˜ ˜OC˜E˜˜˜˜˜X B˜˜OBAX.E˜O
C              PA˜MEPHOCT˜ ˜O˜˜HA ˜˜T˜ HE MEH˜˜E 3+6*NEQN
C     IWORK(*)-˜E˜˜˜ MACC˜B,CO˜EP˜A˜˜˜ ˜H˜OPMA˜˜˜,BH˜TPEHH˜˜ ˜˜˜
C              RKF45,KOTOPA˜ HEO˜XO˜˜MA ˜P˜ ˜OC˜E˜˜˜˜˜X B˜˜OBAX.
C              E˜O PA˜MEPHOCT˜ ˜O˜˜HA ˜˜T˜ HE MEH˜˜E 5.
C
C     ˜EPBOE O˜PA˜EH˜E K RKF45
C
C     ˜O˜˜˜OBATE˜˜ ˜O˜˜EH ˜PE˜˜CMOTPET˜ B CBOE˜ B˜˜˜BA˜˜E˜
C     ˜PO˜PAMME ˜AM˜T˜ ˜˜˜ C˜E˜˜˜˜˜X MACC˜BOB, ˜˜˜˜P˜P˜˜˜˜X
C     B C˜˜CKE B˜˜OBA- Y(NEQN), WORK(3+6*NEQN), IWORK(5);
C     KPOME TO˜O, OH ˜O˜˜EH O˜'˜B˜T˜ F B O˜EPATOPE EXTERNAL,
C     ˜O˜˜OTOB˜T˜ ˜O˜˜PO˜PAMM˜ F(T,Y,YP) ˜ ˜P˜CBO˜T˜ HA˜A˜˜-
C     H˜E ˜HA˜EH˜˜ ˜APAMETPAM-
C
C     NEQN  -˜˜C˜O ˜HTE˜P˜P˜EM˜X ˜PABHEH˜˜ (NEQN.GE.1)
C     Y(*)  -BEKTOP HA˜A˜˜H˜X ˜C˜OB˜˜
C     T     -HA˜A˜˜HA˜ TO˜KA ˜HTE˜P˜POBAH˜˜,
C            T ˜O˜˜HO ˜˜T˜ ˜EPEMEHHO˜.
C     TOUT  -TO˜KA B˜XO˜A,B KOTOPO˜ H˜˜HO HA˜T˜ ˜HA˜EH˜E
C            PE˜EH˜˜. T=TOUT BO˜MO˜HO ˜˜˜˜ ˜P˜ ˜EPBOM
C            O˜PA˜EH˜˜.B ˜TOM C˜˜˜AE B˜XO˜ ˜˜ RKF45 ˜PO˜-
C            CXO˜˜T CO ˜HA˜EH˜EM ˜APAMETPA IFLAG=2,EC˜˜
C            MO˜HO ˜PO˜O˜˜AT˜ ˜HTE˜P˜POBAH˜E.
C     RELERR-˜PAH˜˜A ˜˜˜ OTHOC˜TE˜˜HO˜ ˜OKA˜˜H˜˜ ˜O˜PE˜HOCTE˜.
C     ABSERR-˜PAH˜˜A ˜˜˜ A˜CO˜˜THO˜    ˜OKA˜˜H˜˜ ˜O˜PE˜HOCTE˜.
C            ˜T˜ ˜PAH˜˜˜ ˜O˜˜H˜ ˜˜T˜ HEOTP˜˜ATE˜˜H˜.
C            RELERR ˜O˜˜HA ˜˜T˜ ˜EPEMEHHO˜,A ABSERR MO˜ET
C            ˜˜T˜ ˜ KOHCTAHTO˜.˜PO˜PAMME, BOO˜˜E ˜OBOP˜
C            HE C˜E˜˜ET ˜A˜ABAT˜ ˜PAH˜˜˜ ˜˜˜ OTHOC˜TE˜˜HO˜
C            O˜˜˜K˜,MEH˜˜˜˜, ˜EM ˜P˜MEPHO 1.E-7. ˜A˜˜ ˜˜˜E˜AT˜
C            TP˜˜HOCTE˜ ,CB˜˜AHH˜X C O˜EH˜ B˜COK˜M˜ ˜A˜POCAM˜
C            K TO˜HOCT˜, ˜PO˜PAMMA TPE˜˜ET,˜TO˜˜ RELERR
C            ˜˜˜A ˜O˜˜˜E, ˜EM HEKOTOP˜˜ ˜APAMETP OTHOC˜TE˜˜HO˜
C            O˜˜˜K˜,B˜˜˜C˜˜EM˜˜ BH˜TP˜ EE ˜ ˜AB˜C˜˜˜˜ OT
C            MA˜˜H˜.B ˜ACTHOCT˜,HE PA˜PE˜AETC˜ ˜A˜AH˜E TO˜˜KO
C            A˜CO˜˜THO˜ O˜˜˜K˜.EC˜˜ ˜E ˜A˜AHO ˜HA˜EH˜E RELERR,
C            MEH˜˜EE ˜O˜˜CT˜MO˜O, TO RKF45 ˜BE˜˜˜˜BAET RELERR
C            HA˜˜E˜A˜˜M O˜PA˜OM ˜ BO˜BPA˜AET ˜˜PAB˜EH˜E ˜O˜˜-
C            ˜OBATE˜˜, ˜PE˜˜E ˜EM ˜PO˜O˜˜AT˜ ˜HTE˜P˜POBAH˜E.
C     IFLAG-+1,-1.˜TO ˜KA˜ATE˜˜ HACTPO˜K˜ ˜PO˜PAMM˜ ˜˜˜ KA˜˜O˜
C            HOBO˜ ˜A˜A˜˜. HOPMA˜˜HOE BXO˜HOE ˜HA˜EH˜E PABHO+1.
C            ˜O˜˜˜OBATE˜˜ ˜O˜˜EH ˜A˜ABAT˜ IFLAG=-1 ˜˜˜˜ B TOM
C            C˜˜˜AE,  KO˜˜A HEO˜XO˜˜MO ˜˜PAB˜EH˜E O˜HO˜A˜OB˜M
C            ˜HTE˜PATOPOM.B ˜TOM C˜˜˜AE RKF45 ˜˜TAETC˜ ˜PO˜O˜˜˜T˜
C            PE˜EH˜E HA O˜˜H ˜A˜ B HA˜PAB˜EH˜˜ TOUT ˜P˜ KA˜˜OM
C            O˜EPE˜HOM B˜˜OBE. ˜OCKO˜˜K˜ ˜TOT PE˜˜M PA˜OT˜
C            BEC˜MA HE˜KOHOM˜˜EH, E˜O C˜E˜˜ET ˜P˜MEH˜T˜
C            ˜˜˜˜ B C˜˜˜AE KPA˜HE˜ HEO˜XO˜˜MOCT˜.
C
C     ˜H˜OPMA˜˜˜ HA B˜XO˜E
C
C     Y(*)    -PE˜EH˜E B TO˜KE T
C     T       -˜OC˜E˜H˜˜ TO˜KA,˜OCT˜˜H˜TA˜ ˜P˜ ˜HTE˜P˜POBAH˜˜.
C     IFLAG=2 -˜P˜˜HTE˜P˜POBAH˜˜ ˜OCT˜˜H˜TO TOUT.˜TO ˜HA˜EH˜E
C              ˜APAMETPA ˜KA˜˜BAET HA ˜C˜E˜H˜˜ B˜XO˜ ˜
C              ˜B˜˜ETC˜ HOPMA˜˜H˜M PE˜˜MOM ˜˜˜ ˜PO˜O˜˜EH˜˜
C              ˜HTE˜P˜POBAH˜˜.
C          =3 -˜HTE˜P˜POBAH˜E HE ˜˜˜O ˜AKOH˜EHO ˜˜-˜A TO˜O,
C              ˜TO ˜A˜AHHOE ˜HA˜EH˜E ˜PAH˜˜˜ ˜˜˜ OTHOC˜TE˜˜HO˜
C              O˜˜˜K˜ OKA˜A˜OC˜ C˜˜˜KOM MA˜O. ˜˜˜ ˜PO˜O˜˜EH˜˜
C              ˜HTE˜P˜POBAH˜˜ RELERR ˜˜˜O HA˜˜E˜A˜˜M O˜PA˜OM
C              ˜BE˜˜˜EHO.
C          =4 -˜HTE˜P˜POBAH˜E HE ˜˜˜O ˜AKOH˜EHO ˜˜-˜A TO˜O,
C              ˜TO ˜OTPE˜OBA˜OC˜ ˜O˜EE 3000 B˜˜˜C˜EH˜˜ ˜PO-
C              ˜˜BO˜HO˜.˜TO COOTBETCTBYET ˜P˜˜˜˜˜˜TE˜˜HO
C              500 ˜A˜AM.
C          =5 -˜HTE˜P˜POBAH˜E HE ˜˜˜O ˜AKOH˜EHO ˜˜-˜A TO˜O,
C              ˜TO PE˜EH˜E O˜PAT˜˜OC˜ B HY˜˜,BC˜E˜CTB˜E ˜E˜O
C              TECT TO˜˜KO OTHOC˜TE˜˜HO˜ O˜˜˜K˜ HE ˜POXO˜˜T.
C              ˜˜˜ ˜PO˜O˜˜EH˜˜ HEO˜XO˜˜MO HEHY˜EBOE ˜HA˜EH˜E
C              ˜APAMETPA ABSERR. ˜C˜O˜˜˜OBAH˜E HA O˜˜H ˜A˜
C              PE˜˜MA ˜O˜A˜OBO˜O ˜HTE˜P˜POBAH˜˜ ˜B˜˜ETC˜
C              PA˜YMH˜M B˜XO˜OM ˜˜ ˜O˜O˜EH˜˜.
C          =6 -˜HTE˜P˜POBAH˜E  HE ˜˜˜O ˜AKOH˜EHO ˜˜-˜A TO˜O,
C              ˜TO TPE˜YEMA˜ TO˜HOCT˜ HE MO˜˜A ˜˜T˜ ˜OCT˜˜H˜TA
C              ˜A˜E ˜P˜ HA˜MEH˜˜E˜ ˜O˜˜CT˜ MO˜ BE˜˜˜˜HE ˜A˜A.
C              ˜O˜˜˜OBATE˜˜ ˜O˜˜EH ˜BE˜˜˜˜T˜ ˜PAH˜˜˜ ˜O˜PE˜-
C              HOCT˜,˜PE˜˜E ˜EM MO˜HO ˜˜˜ET ˜O˜˜TAT˜C˜
C              ˜PO˜O˜˜AT˜ ˜HTE˜P˜POBAH˜E.
C          =7 -˜O BCE˜ B˜˜˜MOCT˜, RKF45 HE˜˜˜EKT˜BHA ˜P˜
C              PE˜EH˜˜ ˜TO˜ ˜A˜A˜˜. C˜˜˜KOM ˜O˜˜˜OE ˜˜C˜O
C              TPE˜˜EM˜X B˜XO˜H˜X TO˜EK ˜PE˜˜TCTB˜ET B˜˜OP˜
C              ECTECTBEHHO˜ BE˜˜˜˜H˜ ˜A˜A.C˜E˜˜ET ˜C˜O˜˜˜OBAT˜
C              PE˜˜M ˜O˜A˜OBO˜O ˜HTE˜P˜POBAH˜˜.
C          =8 -HE˜PAB˜˜˜HOE ˜A˜AH˜E BXO˜H˜X ˜APAMETPOB.˜TO
C              ˜HA˜EH˜E ˜O˜B˜˜ETC˜,EC˜˜ ˜O˜˜˜EHA O˜HA ˜˜
C              C˜E˜˜˜˜˜X O˜˜˜OK-
C                              NEQN.LE.0
C                  T=TOUT  ˜  IFLAG.NE.+1  ˜˜˜ -1
C                  RELERR  ˜˜˜  ABSERR.LT.0
C                  IFLAG.EQ.0  ˜˜˜ .LT.-2  ˜˜˜ .GT.8
C     WORK(*) -˜H˜OPMA˜˜˜, KOTOPA˜ O˜˜˜HO HE ˜PE˜CTAB˜˜ET ˜HTE-
C              PECA ˜˜˜ ˜O˜˜˜OBATE˜˜, HO HEO˜XO˜˜MA ˜P˜ ˜OC˜E-
C              ˜˜˜˜˜X B˜˜OBAX. WORK(1),...,WORK(NEQN) CO˜EP˜AT
C              ˜EPB˜E ˜PO˜˜BO˜H˜E BEKTOPA PE˜EH˜˜ Y B TO˜KE T.
C              WORK(NEQN+1) XPAH˜T BE˜˜˜˜H˜ ˜A˜A H,C KOTOPO˜
C              MO˜HO ˜O˜˜TAT˜C˜ ˜POBECT˜ C˜E˜˜˜˜˜˜ ˜A˜.
C     IWORK(*) -˜H˜OPMA˜˜˜, KOTOPA˜ O˜˜˜HO HE ˜PE˜CTAB˜˜ET ˜HTE-
C               PECA ˜˜˜ ˜O˜˜˜OBATE˜˜, HO HEO˜XO˜˜MA ˜P˜ ˜OC˜E-
C               ˜˜˜˜˜X B˜˜OBAX. B IWORK(1) CO˜EP˜˜TC˜
C               C˜ET˜˜K ˜˜C˜A B˜˜˜C˜EH˜˜ ˜PO˜˜BO˜H˜X.
C
C     ˜OC˜E˜˜˜˜˜E O˜PA˜EH˜˜ K RKF45
C
C          HA B˜XO˜E ˜O˜˜PO˜PAMM˜ RKF45 ˜MEETC˜ BC˜ ˜H˜OPMA˜˜˜,
C     HEO˜XO˜˜MA˜  ˜˜˜ ˜PO˜O˜˜EH˜˜ ˜HTE˜P˜POBAH˜˜.EC˜˜ ˜P˜
C     ˜HTE˜P˜POBAH˜˜ ˜OCT˜˜H˜TO TOUT,TO ˜O˜˜˜OBATE˜˜ ˜OCTA-
C     TO˜HO O˜PE˜E˜˜T˜ HOBOE ˜HA˜EH˜E  TOUT ˜ CHOBA O˜PAT˜T˜-
C     C˜ K RKF45.
C          B PE˜˜ME ˜O˜A˜OBO˜O ˜HTE˜P˜POBAH˜˜ (IFLAG=-2)
C     ˜O˜˜˜OBATE˜˜ ˜O˜˜EH ˜MET˜ B B˜˜˜,˜TO KA˜˜˜˜ ˜A˜
C     B˜˜O˜H˜ETC˜ B HA˜PAB˜EH˜˜ TEK˜˜E˜O ˜HA˜EH˜˜ TOUT
C     (C˜˜HA˜˜˜˜P˜EMOM  ˜˜MEHEH˜EM IFLAG HA 2). ˜O˜˜˜OBATE˜˜
C     ˜O˜˜EH ˜A˜AT˜ HOBOE ˜HA˜EH˜E TOUT ˜ ˜EPEO˜PE˜E˜˜T˜
C     IFLAG HA -2, ˜TO˜˜ ˜PO˜O˜˜AT˜ B PE˜˜ME ˜O˜A˜OBO˜O
C     ˜HTE˜P˜POBAH˜˜.
C          EC˜˜ ˜HTE˜P˜POBAH˜E HE ˜˜˜O ˜AKOH˜EHO,HO
C     ˜O˜˜˜OBATE˜˜ XO˜ET ˜PO˜O˜˜AT˜ (C˜˜˜A˜ IFLAG=3,4), OH
C     ˜O˜POCT˜ CHOBA O˜PA˜AETC˜ K RKF45.˜P˜ IFLAG=3 ˜APA-
C     METP RELERR ˜˜˜  ˜˜MEHEH HA˜˜E˜A˜˜M ˜˜˜ ˜PO˜O˜˜EH˜˜
C     ˜HTE˜P˜POBAH˜˜ O˜PA˜OM.B C˜˜˜AE IFLAG=4 C˜ET˜˜K
C     ˜˜C˜A ˜HA˜EH˜˜ ˜˜HK˜˜˜ ˜˜˜ET ˜EPEO˜PE˜E˜EH HA 0, ˜
C     ˜˜˜˜T PA˜PE˜EH˜ E˜E 3000 B˜˜˜C˜EH˜˜ ˜˜HK˜˜˜.
C          O˜HAKO B C˜˜˜AE IFLAG=5, ˜PE˜˜E ˜EM MO˜HO ˜˜˜ET
C     ˜PO˜O˜˜AT˜ ˜HTE˜P˜POBAH˜E,˜O˜˜˜OBATE˜˜ ˜O˜˜EH CHA˜A˜A
C     ˜˜MEH˜T˜ KP˜TEP˜˜ O˜˜˜K˜, ˜A˜AB ˜O˜O˜˜TE˜˜HOE ˜HA˜EH˜E
C     ˜˜˜ ABSERR. EC˜˜ OH HE C˜E˜AET ˜TO˜O, B˜˜O˜HEH˜E ˜PO-
C     ˜PAMM˜ ˜˜˜ET ˜PEKPA˜EHO.
C        TO˜HO TAK ˜E,B C˜˜˜AE IFLAG=6,˜PE˜˜E ˜EM ˜PO˜O˜-
C     ˜AT˜ ˜HTE˜P˜POBAH˜E,˜O˜˜˜OBATE˜˜ HEO˜XO˜˜MO ˜EPEO˜PE-
C     ˜E˜˜T˜ IFLAG HA 2 (˜˜˜ -2, EC˜˜ ˜C˜O˜˜˜˜ETC˜ PE˜˜M
C     ˜O˜A˜OBO˜O ˜HTE˜P˜POBAH˜˜) ˜ ˜BE˜˜˜˜T˜ ˜HA˜EH˜E ˜˜˜
C     ABSERR ˜˜˜O RELERR,˜˜˜O ˜ ˜˜˜ TO˜O,˜ ˜˜˜ ˜P˜˜O˜O.
C     EC˜˜ ˜TO HE ˜˜˜ET C˜E˜AHO,B˜˜O˜HEH˜E ˜PO˜PAMM˜
C     ˜PEKPA˜AETC˜. ˜O˜B˜EH˜E IFLAG=6 ˜KA˜˜BAET HA HEPE˜˜-
C     ˜˜PHOCT˜ (PE˜EH˜E ˜˜CTPO MEH˜ETC˜ ˜˜˜, BO˜MO˜HO,
C     ˜MEETC˜ OCO˜EHHOCT˜),˜ ˜ACTO B ˜O˜O˜H˜X C˜˜˜A˜X
C     HE ˜MEET CM˜C˜A ˜PO˜O˜˜AT˜ ˜HTE˜P˜POBAH˜E.
C          EC˜˜ ˜˜˜ET ˜O˜˜˜EHO ˜HA˜EH˜E IFLAG=7,TO ˜O˜˜˜O-
C     BATE˜˜ ˜O˜˜EH ˜EPE˜T˜ K PE˜˜M˜ ˜O˜A˜OBO˜O ˜HTE˜P˜PO-
C     BAH˜˜ C BE˜˜˜˜HO˜ ˜A˜A,O˜PE˜E˜˜EMO˜ ˜PO˜PAMMO˜, ˜˜˜
C     PACCMOTPET˜ BO˜MO˜HOCTT˜ ˜EPEXO˜A HA ˜PO˜PAMM˜ METO˜OB
C     A˜AMCA.EC˜˜ BCE ˜E ˜O˜˜˜OBATE˜˜ XO˜ET ˜PO˜O˜˜AT˜
C     ˜HTE˜P˜POBAH˜E ˜O ˜O˜˜PO˜PAMME RKF45,OH ˜O˜˜EH ˜O HOBO˜O
C     O˜PA˜EH˜˜ K HE˜ ˜EPEO˜PE˜E˜˜T˜ IFLAG HA 2.B ˜POT˜BHOM
C     C˜˜˜AE B˜˜O˜HEH˜E ˜PO˜PAMM˜ ˜˜˜ET ˜PEKPA˜EHO.
C          EC˜˜ ˜O˜˜˜EHO ˜HA˜EH˜E IFLAG=8,TO ˜HTE˜P˜POBAH˜E
C     HE˜˜˜˜ ˜PO˜O˜˜AT˜,˜OKA HE ˜˜˜˜T ˜C˜PAB˜EH˜ O˜˜˜O˜H˜E
C     BXO˜H˜E ˜APAMETP˜. H˜˜HO OTMET˜T˜,˜TO MACC˜B˜ WORK ˜
C     IWORK CO˜EP˜AT ˜H˜OPMA˜˜˜,HEO˜XO˜˜M˜˜ ˜˜˜ ˜A˜˜HE˜˜E˜O
C     ˜HTE˜P˜POBAH˜˜.˜O˜TOM˜ B ˜T˜ MACC˜B˜ HE˜˜˜˜ BHOC˜T˜
C     ˜˜MEHEH˜˜.
C
      EXTERNAL F
      INTEGER NEQN,IFLAG,IWORK(5)
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,WORK(1)
C
C     EC˜˜ TPAHC˜˜TOP ˜POBEP˜ET ˜H˜EKC˜, TO ˜AMEH˜T˜
C     WORK(1) HA WORK(3+6*NEQN)
C
      INTEGER K1,K2,K3,K4,K5,K6,K1M
C
C     B˜˜˜C˜˜T˜ ˜H˜EKC˜ ˜˜˜ PAC˜E˜˜EH˜˜ PA˜O˜E˜O MACC˜BA
C
      K1M=NEQN+1
      K1=K1M+1
      K2=K1+NEQN
      K3=K2+NEQN
      K4=K3+NEQN
      K5=K4+NEQN
      K6=K5+NEQN
C
C     ˜TA ˜POME˜YTO˜HA˜ ˜PO˜PAMMA ˜POCTO COKPA˜AET ˜˜˜
C     ˜O˜˜˜OBATE˜˜ ˜˜˜HH˜˜ C˜˜COK B˜˜OBA ˜YTEM PAC˜E˜˜EH˜˜
C     ˜BYX PA˜O˜˜X MACC˜BOB. EC˜˜ ˜TO HE COBMECT˜MO C
C     TPAHC˜˜TOPOM,TO OH ˜O˜˜EH O˜PA˜AT˜C˜ HE˜OCPE˜CTBEHHO
C     K ˜O˜˜PO˜PAMME RKFS .
C
      CALL RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *          WORK(1),WORK(K1M),WORK(K1),WORK(K2),
     *          WORK(K3),WORK(K4),WORK(K5),WORK(K6),
     *          WORK(K6+1),IWORK(1),IWORK(2),
     *          IWORK(3),IWORK(4),IWORK(5))
      RETURN
      END
