package com.ibm.wcaz.implementation;


import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;


public class JdbcConnection {
    
    static final String JDBC_DRIVER = "com.ibm.db2.jcc.DB2Driver";
    static final String DB_URL = "jdbc:db2://xxx.xxx.xxx.xxx:xxxx/XXXXX";

    //  Database credentials
    static final String USER = "xxxx";
    static final String PASS = "xxxx";

    public static Connection connection() throws ClassNotFoundException, SQLException {
        //STEP 2: Register JDBC driver
        Class.forName(JDBC_DRIVER);

        //STEP 3: Open a connection
        Connection conn = null;
        
        conn = DriverManager.getConnection(DB_URL, USER, PASS);
        
        conn.setAutoCommit(false);//This is added to improve the performance of application

        
        return conn;
    }
}

