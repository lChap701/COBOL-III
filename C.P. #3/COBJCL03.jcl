//KC03A093 JOB  (1234,COBOLIII),CHAPMAN,NOTIFY=&SYSUID,MSGCLASS=H,
//             COND=(8,LE)
//***************************************************
//* THIS STEP DELETES OLD COPIES OF THE GDG DATASET *
//* THIS STEP WAS CREATED BY LUCAS CHAPMAN          *
//***************************************************
//STEP1    EXEC PGM=IEFBR14
//DD1      DD   DSN=KC03A09.GRADE.DAT.G0001V00,
//             DISP=(OLD,DELETE)
//DD2      DD   DSN=KC03A09.GRADE.DAT.G0002V00,
//             DISP=(OLD,DELETE)
//DD3      DD   DSN=KC03A09.GRADE.DAT.G0003V00,
//             DISP=(OLD,DELETE)
//DD4      DD   DSN=KC03A09.GRADE.DAT.G0004V00,
//             DISP=(OLD,DELETE)
//********************************************
//* THIS STEP DELETES THE BASE GDG DATASET   *
//* THIS STEP WAS CREATED BY LUCAS CHAPMAN   *
//********************************************
//STEP2    EXEC PGM=IDCAMS,COND=EVEN
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
 DELETE (KC03A09.GRADE.DAT) GENERATIONDATAGROUP
/*
//*******************************************
//* THIS STEP CREATES A BASE GDG DATASET    *
//* THIS STEP WAS CREATED BY NATAHAN EASTEP *
//*******************************************
//STEP3    EXEC PGM=IDCAMS,COND=EVEN
//SYSPRINT DD   SYSOUT=*
//MODEL    DD   DSN=KC03A09.GRADES.DAT,
//             DISP=(OLD,CATLG,KEEP)
//SYSIN    DD   *
 DEFINE GDG(NAME(KC03A09.GRADE.DAT) -
            LIMIT(5)                -
            NOEMPTY                 -
            SCRATCH)
/*
//*******************************************************************
//* THIS STEP CREATES COPIES OF THE GDG DATASET AND A DUMMY DATASET *
//* THIS STEP WAS CREATED BY LUCAS CHAPMAN & ALONSO                 *
//*******************************************************************
//STEP4    EXEC PGM=IEFBR14
//DD1      DD   DSN=KC03A09.GRADE.DAT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=KC03A09.GRADES.DAT
//DD2      DD   DSN=KC03A09.GRADE.DAT(+2),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=KC03A09.GRADES.DAT
//DD3      DD   DSN=KC03A09.GRADE.DAT(+3),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=KC03A09.GRADES.DAT
//DD4      DD   DSN=KC03A09.GRADE.DAT(+4),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=KC03A09.GRADES.DAT
//DD5      DD   DUMMY,DSN=KC03A09.GRADE.DAT(+5),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(1,1)),
//             DCB=KC03A09.GRADES.DAT
//******************************************
//* THIS STEP COPIES DATA IN DATASETS      *
//* THIS STEP WAS CREATED BY LUCAS CHAPMAN *
//******************************************
//STEP5    EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
 REPRO -
       INDATASET(KC03A09.GRADES.DAT) -
       OUTDATASET(KC03A09.GRADE.DAT.G0001V00)
 REPRO -
       INDATASET(KC03A09.GRADE.DAT.G0001V00)  -
       OUTDATASET(KC03A09.GRADE.DAT.G0002V00) -
       COUNT(10)
 REPRO -
       INDATASET(KC03A09.GRADE.DAT.G0002V00)  -
       OUTDATASET(KC03A09.GRADE.DAT.G0003V00) -
       COUNT(5)
 REPRO -
       INDATASET(KC03A09.GRADE.DAT.G0003V00)  -
       OUTDATASET(KC03A09.GRADE.DAT.G0004V00) -
       COUNT(1)
/*
//******************************************
//* THIS STEP PRINTS DATA IN DATASETS      *
//* THIS STEP WAS CREATED BY LUCAS CHAPMAN *
//******************************************
//STEP6    EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
 PRINT -
       INDATASET(KC03A09.GRADE.DAT.G0001V00) -
       CHARACTER
 PRINT -
       INDATASET(KC03A09.GRADE.DAT.G0002V00) -
       CHARACTER
 PRINT -
       INDATASET(KC03A09.GRADE.DAT.G0003V00) -
       CHARACTER
 PRINT -
       INDATASET(KC03A09.GRADE.DAT.G0004V00) -
       CHARACTER
/*
//******************************************************************
//* THIS STEP USES LISTCAT TO SHOW CHANGES MADE TO THE GDG DATASET *
//* THIS STEP WAS CREATED BY LUCAS CHAPMAN                         *
//******************************************************************
//STEP7    EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
 LISTCAT ALL ENTRIES(KC03A09.GRADE.DAT)
/*
//