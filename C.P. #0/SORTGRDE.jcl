//KC03A09G JOB  (1234,GRADES),CHAPMAN,NOTIFY=KC03A09,MSGCLASS=H
//* SORTS THE GRADES BY COURSE ID AND STUDENT ID
//SORT1    EXEC PGM=SORT
//SORTIN   DD   DSN=KC03A09.GRADES.DAT,DISP=SHR
//SORTLIB  DD   DSN=SYS1.SORTLIB,DISP=SHR
//SORTOUT  DD   DSN=KC03A09.SORTED.GRADES,DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(1,1)),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=31,BLKSIZE=310)
//SORTWK01 DD   UNIT=SYSDA,SPACE=(TRK,(1,1))
//*SYSIN    DD   *
//SYSOUT   DD   SYSOUT=*
 SORT FIELDS=(1,8,CH,D,9,7,CH,A)
/*
//