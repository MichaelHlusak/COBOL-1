//KC03CEAA    JOB (PROG4,HLUSAK),CLASS = A,MSGCLASS = H                 
//            EXEC IGYWCLG                                              
//COBOL.SYSIN DD *                                                      
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PROG4.                                               
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT EMPLOYEE-DATA ASSIGN INDD.                           
            SELECT DATAOUT      ASSIGN OUTDD.                           
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  EMPLOYEE-DATA    RECORDING MODE F                            
                            LABEL RECORDS ARE OMITTED.                  
       01   EMPIN.                                                      
              02 LAST-NAME-IN       PIC X(10).                          
              02 HOURS-WORKED-IN    PIC 99.                             
              02 PAY-RATE-IN        PIC 999V99.                         
              02 NUM-DEPENDENTS-IN  PIC 99.                             
              02 EMP-TYPE-IN        PIC X(1).                           
              02 YEAR-HIRED-IN      PIC 9(04).                          
              02 FILLER             PIC X(56).                          
       FD DATAOUT           RECORDING MODE F                            
                            LABEL RECORDS ARE OMITTED.                  
       01 PRINTOUT PIC X(100).                                          
       WORKING-STORAGE SECTION.                                         
       01 EOF PIC XXX VALUE "NO".                                       
       01 RECORD-OUT.                                                   
           02 LAST-NAME-OUT        PIC  X(10).                          
           02 FILLER               PIC  X VALUE SPACE.                  
           02 HOURS-WORKED-OUT     PIC  99.                             
           02 FILLER               PIC  X(3) VALUE SPACE.               
           02 PAY-RATE-OUT         PIC  $$9.99.                         
           02 OT-PAY-OUT           PIC  $$$99.99.                       
           02 FILLER               PIC  XX VALUE SPACE.                 
           02 COLA-PAY-OUT         PIC  $$$$9.99.                       
           02 FILLER               PIC  X(4) VALUE SPACE.               
           02 GROSS-PAY-OUT        PIC  $$$$9.99.                       
           02 FILLER               PIC  X(3) VALUE SPACE.               
           02 DEPEN-DEDUCT-AMT-OUT PIC  $$$$9.99.                       
           02 FILLER               PIC  X(4) VALUE SPACE.               
           02 FEDERAL-TAXES-OUT    PIC  $$$$9.99.                       
           02 FILLER               PIC  X VALUE SPACE.                  
           02 STATE-TAXES-OUT      PIC  $$$$9.99.                       
           02 FILLER               PIC  XX VALUE SPACE.                 
           02 NET-PAY-OUT          PIC  $$$$9.99.                       
           02 FILLER               PIC  X(05).                          
       01 HEADER-LINE-ONE.                                              
          02  FILLER    PIC X(10)  VALUE "EMPLOYEE".                    
          02  FILLER    PIC X(08)  VALUE "HOURS".                       
          02  FILLER    PIC X(06)  VALUE "PAY".                         
          02  FILLER    PIC X(11)  VALUE "OVER".                        
          02  FILLER    PIC X(12)  VALUE "COLA".                        
          02  FILLER    PIC X(11)  VALUE "GROSS".                       
          02  FILLER    PIC X(11)  VALUE "DEPENDENT".                   
          02  FILLER    PIC X(10)  VALUE "FRDERAL".                     
          02  FILLER    PIC X(09)  VALUE "STATE".                       
          02  FILLER    PIC X(09)  VALUE "NET".                         
       01 HEADER-LINE-TWO.                                              
          02  FILLER    PIC X(10)  VALUE "NAME".                        
          02  FILLER    PIC X(08)  VALUE "WORKED".                      
          02  FILLER    PIC X(06)  VALUE "RATE".                        
          02  FILLER    PIC X(11)  VALUE "TIME-PAY".                    
          02  FILLER    PIC X(12)  VALUE "ADJUSTMENT".                  
          02  FILLER    PIC X(11)  VALUE "PAY".                         
          02  FILLER    PIC X(11)  VALUE "DEDUCTION".                   
          02  FILLER    PIC X(10)  VALUE "TAXES".                       		   
          02  FILLER    PIC X(09)  VALUE "TAXES".                      
          02  FILLER    PIC X(09)  VALUE "PAY".                        
       01 BASE-HRS-SAVE    PIC       99.                               
       01 OT-HRS-SAVE      PIC       99.                               
       01 BASE-PAY-SAVE    PIC 99999V99.                               
       01 OT-PAY-SAVE      PIC 99999V99.                               
       01 OT-PERCT         PIC   999V99.                               
       01 COLA-PAY-SAVE    PIC  9999V99.                               
       01 GROSS-PAY-SAVE   PIC 99999V99.                               
       01 DEPEND-SAVE      PIC   999V99.                               
       01 TAXABLE-INC-SAVE PIC  9999V99.                               
       01 FED-TAX-SAVE     PIC   999V99.                               
       01 STATE-TAX-SAVE   PIC   999V99.                               
       01 NET-PAY-SAVE     PIC  9999V99.                               
      *                                                                
       PROCEDURE DIVISION.                                             
       MAIN-MODULE.                                                    
           OPEN INPUT EMPLOYEE-DATA OUTPUT DATAOUT.                    
           WRITE PRINTOUT FROM HEADER-LINE-ONE                         
           WRITE PRINTOUT FROM HEADER-LINE-TWO                         
           PERFORM READ-RECORD.                                        
           PERFORM UNTIL EOF = "YES"                                   
               MOVE LAST-NAME-IN TO LAST-NAME-OUT                      
               MOVE HOURS-WORKED-IN TO HOURS-WORKED-OUT                
               MOVE PAY-RATE-IN TO PAY-RATE-OUT                        
                PERFORM DEBUG-INPUTS-IN                                
                PERFORM SEPERATE-HRS                                   
                PERFORM CALC-PAY                                       
                PERFORM CALC-COLA-PAY                                  
                PERFORM CALC-GROSS                                     
                PERFORM CALC-DEP-DEDUCTION-AMT                         
                PERFORM CALC-TAXABLE-INCOME                            
                PERFORM CALC-FED-TAXES                                 
                PERFORM CALC-STATE-TAXES                               
                PERFORM CALC-NET-PAY                                   		  
               MOVE GROSS-PAY-SAVE TO GROSS-PAY-OUT                     
               MOVE DEPEND-SAVE TO DEPEN-DEDUCT-AMT-OUT                 
               MOVE OT-PAY-SAVE TO OT-PAY-OUT                           
               MOVE COLA-PAY-SAVE TO COLA-PAY-OUT                       
               MOVE FED-TAX-SAVE TO FEDERAL-TAXES-OUT                   
               MOVE STATE-TAX-SAVE TO STATE-TAXES-OUT                   
               MOVE NET-PAY-SAVE TO NET-PAY-OUT                         
                WRITE PRINTOUT FROM RECORD-OUT                          
               PERFORM READ-RECORD                                      
               PERFORM DEBUG-DATA-OUT                                   
           END-PERFORM.                                                 
           CLOSE EMPLOYEE-DATA, DATAOUT.                                
           STOP RUN.                                                    
       SEPERATE-HRS.                                                    
            IF HOURS-WORKED-IN > 40                                     
             COMPUTE OT-HRS-SAVE = HOURS-WORKED-IN - 40                 
             COMPUTE BASE-HRS-SAVE = 40                                 
            ELSE                                                        
               IF HOURS-WORKED-IN  = 00                                 
                 COMPUTE OT-HRS-SAVE = 00                               
                 COMPUTE BASE-HRS-SAVE = 00                             
               ELSE                                                     
                 COMPUTE OT-HRS-SAVE = 00                               
                 COMPUTE BASE-HRS-SAVE = HOURS-WORKED-IN                
               END-IF                                                   
            END-IF.                                                     
       CALC-PAY.                                                        
            IF EMP-TYPE-IN = "P"                                        
             COMPUTE OT-PAY-SAVE = OT-HRS-SAVE * 1.5 * PAY-RATE-IN      
             COMPUTE BASE-PAY-SAVE = BASE-HRS-SAVE * PAY-RATE-IN        
            ELSE                                                        
               IF EMP-TYPE-IN = "T"                                     
                 COMPUTE OT-PAY-SAVE = OT-HRS-SAVE * PAY-RATE-IN        
                 COMPUTE BASE-PAY-SAVE = BASE-HRS-SAVE * PAY-RATE-IN    
               ELSE                         
                 COMPUTE OT-PAY-SAVE = 00                               
                 COMPUTE BASE-PAY-SAVE = 00                             
               END-IF                                                   
            END-IF.                                                     
            DISPLAY "STUB..".                                           
       CALC-COLA-PAY.                                                   
            IF YEAR-HIRED-IN < 2010                                     
             COMPUTE COLA-PAY-SAVE = BASE-PAY-SAVE * .05                
            ELSE                                                        
             COMPUTE COLA-PAY-SAVE = 0.00                               
            END-IF.                                                     
       CALC-DEP-DEDUCTION-AMT.                                          
            COMPUTE DEPEND-SAVE = NUM-DEPENDENTS-IN * 25.               
       CALC-TAXABLE-INCOME.                                             
            COMPUTE TAXABLE-INC-SAVE = GROSS-PAY-SAVE - DEPEND-SAVE.    
       CALC-FED-TAXES.                                                  
            IF NUM-DEPENDENTS-IN > 0                                    
              COMPUTE FED-TAX-SAVE = TAXABLE-INC-SAVE * .20             
            ELSE                                                        
              COMPUTE FED-TAX-SAVE = TAXABLE-INC-SAVE * .25             
            END-IF.                                                     
       CALC-STATE-TAXES.                                                
            COMPUTE STATE-TAX-SAVE = TAXABLE-INC-SAVE * .10.            
       CALC-GROSS.                                                      
            COMPUTE GROSS-PAY-SAVE = BASE-PAY-SAVE + OT-PAY-SAVE +      
                                     COLA-PAY-SAVE.                     
       CALC-NET-PAY.                                                    
            COMPUTE NET-PAY-SAVE = GROSS-PAY-SAVE - FED-TAX-SAVE -      
                                   STATE-TAX-SAVE.                      
       DEBUG-INPUTS-IN.                                                 
           DISPLAY "                                             "      
           DISPLAY "------------INPUT-IN-------------------------"      
           DISPLAY "LAST-NAME-IN  ------->" LAST-NAME-IN                
           DISPLAY "HOURS-WORKED-IN------>" HOURS-WORKED-IN             
           DISPLAY "PAY-RATE-IN---------->" PAY-RATE-IN                 			   
           DISPLAY "PAY-RATE-IN---------->" PAY-RATE-IN                 
           DISPLAY "NUM-DEPENDENTS-IN---->" NUM-DEPENDENTS-IN           
           DISPLAY "EMP-TYPE-IN---------->" EMP-TYPE-IN                 
           DISPLAY "YEAR-HIRED-IN-------->" YEAR-HIRED-IN               
           DISPLAY "--------END-INPUTSIN-------------------------"      
           DISPLAY "                                             ".     
       DEBUG-DATA-OUT.                                                  
           DISPLAY "                                             "      
           DISPLAY "------------OUTPUT-OUT-----------------------"      
           DISPLAY "HOURS-WORKED-OUT----->" HOURS-WORKED-OUT            
           DISPLAY "--------------------->"                             
           DISPLAY "---OT-HRS-SAVE------->" OT-HRS-SAVE                 
           DISPLAY "---OT-PAY-SAVE------->" OT-PAY-SAVE                 
           DISPLAY "--------------------->"                             
           DISPLAY "---BASE-HRS-SAVE----->" BASE-HRS-SAVE               
           DISPLAY "---BASE-PAY-SAVE----->" BASE-PAY-SAVE               
           DISPLAY "--------------------->"                             
           DISPLAY "---COLA-PAY-SAVE----->" COLA-PAY-SAVE               
           DISPLAY "TAXABLE-INC-SAVE----->" TAXABLE-INC-SAVE            
           DISPLAY "------------END-OUTPUT-OUT-------------------"      
           DISPLAY "                                             ".     
       READ-RECORD.                                                     
           READ EMPLOYEE-DATA AT END MOVE "YES" TO EOF                  
           END-READ.                                                    
/*                                                                      
//GO.INDD  DD  *                                                        
HLUSAK    500100001P2001                                                
FORMAN    500100001T2001                                                
SHIAMZ    500100000P2012                                                
BOGART    500100000T2012                                                
PAKLEFISH 160200015T1976                                                
ALKAFRAZ  250880019P1976                                                
BILLBASS  550050099T2016                                                
MALKOVITCH990777712P1946                                                
SALATZAR  010123101T2001                                                
FARREH    550159946T2010                                                		   
MARSH1    400100000T2012                                                
MARSH2    500100000T2012                                                
MARSH3    400100000P2012                                                
MARSH4    500100000P2012                                                
/*                                                                      
//GO.SYSOUT DD SYSOUT=*                                                 
//GO.SYSUDUMP DD SYSOUT=A                                               
//GO.OUTDD DD SYSOUT=A                                                  