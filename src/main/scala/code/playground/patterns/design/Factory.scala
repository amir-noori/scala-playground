package code.playground.patterns.design

object Factory extends App {


  trait SimpleConnection {
    def getName: String

    def executeQuery(query: String)
  }

  class SimpleMySqlConnection extends SimpleConnection {
    override def getName: String = "MySQL connection"
    override def executeQuery(query: String): Unit = println(s"executing ${query} for MySQL...")
  }

  class SimplePostgresConnection extends SimpleConnection {
    override def getName: String = "Postgres connection"
    override def executeQuery(query: String): Unit = println(s"executing ${query} for Postgres...")
  }

  abstract class DatabaseClient {
    def executeQuery(query: String): Unit = {
      val connection = connect()
      connection.executeQuery(query)
    }
    protected def connect(): SimpleConnection
  }
  class MysqlClient extends DatabaseClient {
    override protected def connect(): SimpleConnection = new SimpleMySqlConnection
  }

  class PgSqlClient extends DatabaseClient {
    override protected def connect(): SimpleConnection = new SimplePostgresConnection
  }

  val clientMySql: DatabaseClient = new MysqlClient
  val clientPgSql: DatabaseClient = new PgSqlClient
  clientMySql.executeQuery("SELECT * FROM users")
  clientPgSql.executeQuery("SELECT * FROM employees")







  trait SimpleConnectionPrinter {
    def printSimpleConnection(connection: SimpleConnection): Unit
  }

  abstract class BadDatabaseClient {
    def executeQuery(query: String): Unit = {
      val connection = connect()
      val connectionPrinter = getConnectionPrinter()
      connectionPrinter.printSimpleConnection(connection)
      connection.executeQuery(query)
    }
    protected def connect(): SimpleConnection
    protected def getConnectionPrinter(): SimpleConnectionPrinter
  }


  class SimpleMySqlConnectionPrinter extends SimpleConnectionPrinter {
    override def printSimpleConnection(connection: SimpleConnection): Unit = {
//      val x= connection.getName()
//      println(s"I require a MySQL connection. It is: ${x}")
    }
  }

  class SimplePgSqlConnectionPrinter extends SimpleConnectionPrinter {
    override def printSimpleConnection(connection: SimpleConnection): Unit = {
//      println(s"I require a PgSQL connection. It is: ${connection.getName()}")
    }
  }


  class BadMySqlClient extends BadDatabaseClient {
    override protected def connect(): SimpleConnection = new SimpleMySqlConnection
    override protected def getConnectionPrinter(): SimpleConnectionPrinter = {
      // return value is correct.
      new SimpleMySqlConnectionPrinter
    }
  }

  class BadPgSqlClient extends BadDatabaseClient {
    override protected def connect(): SimpleConnection = new SimplePostgresConnection
    override protected def getConnectionPrinter(): SimpleConnectionPrinter = {
      // should return SimplePgSqlConnectionPrinter.
      new SimpleMySqlConnectionPrinter
    }
  }

  val clientMySql2: BadDatabaseClient = new BadMySqlClient
  val clientPgSql2: BadDatabaseClient = new BadPgSqlClient
  clientMySql2.executeQuery("SELECT * FROM users")
  clientPgSql2.executeQuery("SELECT * FROM employees")


}
