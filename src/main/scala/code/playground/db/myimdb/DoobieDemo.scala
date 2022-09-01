package code.playground.db.myimdb

import cats.effect.{ExitCode, IO, IOApp}
import doobie.implicits._
import doobie.util.transactor.Transactor
import doobie.{HC, HPS}

object DoobieDemo extends IOApp {

  implicit class Debugger[A](io: IO[A]) {
    def debug: IO[A] = io.map { a =>
      println(s"[${Thread.currentThread().getName}] -> $a")
      a
    }
  }

  case class Actor(id: Int, name: String)

  case class Movie(id: String, title: String, year: Int, actors: List[String], director: String)


  val transactor: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/myimdb",
    "postgres",
    "root123"
  )

  def findActorById(id: Int): IO[Option[Actor]] = {
    val query = sql"select id, name from actors where id=$id".query[Actor]
    val action = query.option
    action.transact(transactor)
  }

  def findActorByName(name: String): IO[Option[Actor]] = {
    val queryString = "select id, name from actors where name = ?"
    HC.stream[Actor](
      queryString,
      HPS.set(name),
      100
    ).compile.toList.map(_.headOption).transact(transactor)
  }

  def findActorsByInitial(letter: String): IO[List[Actor]] = {
    val select = fr"select id, name"
    val from = fr"from actors"
    val where = fr"where LEFT(name, 1) = $letter"
    val statement = select ++ from ++ where
    statement.query[Actor].stream.compile.toList.transact(transactor)
  }

  def findAllActorNames(): IO[List[String]] = {
    val query = sql"select name from actors".query[String]
    val action = query.to[List]
    action.transact(transactor)
  }

  // it is a goof idea to use streams to return lists
  def findAllActorNamesByStream(): IO[List[String]] = {
    sql"select name from actors".query[String].stream.compile.toList.transact(transactor)
  }

  def saveActor(actor: Actor): IO[Int] = {
    val insert = sql"insert into actors(id, name) values (${actor.id}, ${actor.name})"
    insert.update.run.transact(transactor)
  }

  def showAllActorNames(names: List[String]): IO[Unit] = {
    println("names: " + names)
    names.foreach(println)
    IO(names.foreach(println))
  }

  override def run(args: List[String]): IO[ExitCode] =
  //  findActorByName("Henry Cavill").debug.as(ExitCode.Success)
    (
      for {
        names <- findAllActorNames().debug
        _ <- showAllActorNames(names)
      } yield ()
      ).as(ExitCode.Success)
}
