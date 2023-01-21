package code.playground.cats

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Left, Right, Success}

object TraverseCommon {
  def convertDataFuture(value: Int)(implicit ec: ExecutionContext): Future[String] =
    Future {
      Thread.sleep(1000)
      println(s"value -> $value")
      s"value -> $value"
    }

  def convertDataOption(value: Int): Option[String] =
    Some(s"value -> $value")

  def convertDataEither(input: String): Either[String, String] =
    if (input.contains("error")) Left(s"$input -> contains error")
    else Right(input)
}

object Traverse {

  import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def traverseFuture[A, B](as: List[A])(f: A => Future[B])(implicit ec: ExecutionContext): Future[List[B]] =
    Future.traverse(as)(f)

  def main(args: Array[String]): Unit = {
    val result: Future[List[String]] = traverseFuture(List(10, 20, 30))(TraverseCommon.convertDataFuture)

    result.onComplete {
      case Success(value) => println(s"futureResult value is $value")
      case _ => println("error occurred!")
    }

    Thread.sleep(4000)
  }

}


object TraverseCats {

  import cats.implicits._

  import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global


  def main(args: Array[String]): Unit = {
    //    val futureResult: Future[List[String]] = List(10, 20, 30).traverse(TraverseCommon.convertDataFuture)
//    val optionResult: Option[List[String]] = List(10, 20, 30).traverse(TraverseCommon.convertDataOption)
    val eitherResult: Either[String, List[String]] = List("data 1", "some other data", "data with error", "last data")
      .traverse(TraverseCommon.convertDataEither)

    println(eitherResult)


    //    futureResult.onComplete {
    //      case Success(value) => println(s"futureResult value is $value")
    //      case _ => println("error occurred!")
    //    }
    //
    //    Thread.sleep(4000)
  }

}