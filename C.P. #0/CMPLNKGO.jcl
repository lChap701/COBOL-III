//KC03A090 JOB  (1234,COBOLIII),CHAPMAN,NOTIFY=&SYSUID,MSGCLASS=H
//* THIS JOB WILL COMPILE, LINK AND GO A GIVEN PROGRAM
//STEP1    EXEC IGYWCLG,
//             PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM'
//COBOL.SYSIN DD DSN=KC03A09.COBOL.PGMLIB(COBLSC00),
//             DISP=(SHR,KEEP)
//GO.GRADESIN DD DSN=KC03A09.SORTED.GRADES,
//             DISP=(SHR,KEEP)
//GO.GRADEOUT DD DSN=KC03A09.GRADES.PRT,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(1,1)),
//             DCB=(DSORG=PS,LRECL=133,RECFM=FB,BLKSIZE=1330)
//