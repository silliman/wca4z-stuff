
      ******************************************************************        
      *                                                                *        
      * LICENSED MATERIALS - PROPERTY OF IBM                           *        
      *                                                                *        
      * "RESTRICTED MATERIALS OF IBM"                                  *        
      *                                                                *        
      * CB12                                                           *        
      *                                                                *        
      * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *        
      *                                                                *        
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *        
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *        
      * CONTRACT WITH IBM CORPORATION                                  *        
      *                                                                *        
      ******************************************************************        
      *               COPYBOOK for Policy details                      *        
      *                                                                *        
      *   Structures to map values obtained from DB2 tables:           *        
      *   Customer, Policy, Endowment, House and Motor.                *        
      *                                                                *        
      *   All lengths of policy fields will be defined here so that    *        
      *   if any of the DB2 table contents change the lengths will     *        
      *   only need to be changed here.                                *        
      *                                                                *        
      ******************************************************************        
                                                                                
           EXEC SQL DECLARE CUSTOMER TABLE                                      
           ( CUSTOMERNUMBER                 INTEGER NOT NULL,                   
             FIRSTNAME                      CHAR(10),                           
             LASTNAME                       CHAR(20),                           
             DATEOFBIRTH                    DATE,                               
             HOUSENAME                      CHAR(20),                           
             HOUSENUMBER                    CHAR(4),                            
             POSTCODE                       CHAR(8),                            
             PHONEHOME                      Char(20),                           
             PHONEMOBILE                    Char(20),                           
             EMAILADDRESS                   Char(100)                           
           ) END-EXEC.                                                          
                                                                                
           EXEC SQL DECLARE CUSTOMER_SECURE TABLE                               
           ( CUSTOMERNUMBER INTEGER NOT NULL,                                   
             CUSTOMERPASS   CHAR(32),                                           
             STATE_INDICATOR CHAR(1),                                           
             PASS_CHANGES   INTEGER                                             
           ) END-EXEC.                                                          
                                                                                
           EXEC SQL DECLARE POLICY TABLE                                        
           ( POLICYNUMBER     INTEGER NOT NULL,                                 
             CUSTOMERNUMBER   INTEGER NOT NULL,                                 
             ISSUEDATE        DATE,                                             
             EXPIRYDATE       DATE,                                             
             POLICYTYPE       CHAR(1),                                          
             LASTCHANGED      TIMESTAMP NOT NULL,                               
             BROKERID         INTEGER,                                          
             BROKERSREFERENCE CHAR(10),                                         
             PAYMENT          INTEGER,                                          
             COMMISSION       SMALLINT                                          
           ) END-EXEC.                                                          
                                                                                
           EXEC SQL DECLARE ENDOWMENT TABLE                                     
           ( POLICYNUMBER   INTEGER NOT NULL,                                   
             EQUITIES       CHAR(1),                                            
             WITHPROFITS    CHAR(1),                                            
             MANAGEDFUND    CHAR(1),                                            
             FUNDNAME       CHAR(10),                                           
             TERM           SMALLINT,                                           
             SUMASSURED     INTEGER,                                            
             LIFEASSURED    CHAR(31),                                           
             PADDINGDATA    VARCHAR(32606)                                      
           ) END-EXEC.                                                          
                                                                                
           EXEC SQL DECLARE HOUSE TABLE                                         
           ( POLICYNUMBER   INTEGER NOT NULL,                                   
             PROPERTYTYPE   CHAR(15),                                           
             BEDROOMS       SMALLINT,                                           
             VALUE          INTEGER,                                            
             HOUSENAME      CHAR(20),                                           
             HOUSENUMBER    CHAR(4),                                            
             POSTCODE       CHAR(8)                                             
           ) END-EXEC.                                                          
                                                                                
           EXEC SQL DECLARE MOTOR TABLE                                         
           ( POLICYNUMBER   INTEGER NOT NULL,                                   
             MAKE           CHAR(15),                                           
             MODEL          CHAR(15),                                           
             VALUE          INTEGER,                                            
             REGNUMBER      CHAR(7),                                            
             COLOUR         CHAR(8),                                            
             CC             SMALLINT,                                           
             YEAROFMANUFACTURE DATE,                                            
             PREMIUM          INTEGER,                                          
             ACCIDENTS        INTEGER                                           
           ) END-EXEC.                                                          
                                                                                
           EXEC SQL DECLARE COMMERCIAL TABLE                                    
           ( POLICYNUMBER       INTEGER      NOT NULL,                          
             REQUESTDATE        TIMESTAMP            ,                          
             STARTDATE          DATE                 ,                          
             RENEWALDATE        DATE                 ,                          
             ADDRESS            CHAR(255)            ,                          
             ZIPCODE            CHAR(8)              ,                          
             LATITUDEN          CHAR(11)             ,                          
             LONGITUDEW         CHAR(11)             ,                          
             CUSTOMER           CHAR(255)            ,                          
             PROPERTYTYPE       CHAR(255)            ,                          
             FIREPERIL          SMALLINT             ,                          
             FIREPREMIUM        INTEGER              ,                          
             CRIMEPERIL         SMALLINT             ,                          
             CRIMEPREMIUM       INTEGER              ,                          
             FLOODPERIL         SMALLINT             ,                          
             FLOODPREMIUM       INTEGER              ,                          
             WEATHERPERIL       SMALLINT             ,                          
             WEATHERPREMIUM     INTEGER              ,                          
             STATUS             SMALLINT             ,                          
             REJECTIONREASON    CHAR(255)                                       
           ) END-EXEC.                                                          
                                                                                
                                                                                
           EXEC SQL DECLARE CLAIM TABLE                                         
           ( CLAIMNUMBER         INTEGER NOT NULL,                              
             POLICYNUMBER       INTEGER      NOT NULL,                          
             CLAIMDATE          DATE                 ,                          
             PAID               INTEGER              ,                          
             VALUE              INTEGER              ,                          
             CAUSE              CHAR(255)            ,                          
             OBSERVATIONS       CHAR(255)                                       
           ) END-EXEC.                                                          
                
