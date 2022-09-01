package code.playground.typelevel.effect

import cats.effect.{ExitCode, IO, IOApp}


object SimpleFileCopy extends IOApp {

  import cats.effect.{IO, Resource}

  import java.io._

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f)) // build
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f)) // build
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transfer(inStream: InputStream, outStream: OutputStream): IO[Long] = {

    def transferInner(arr: Array[Byte], acc: Long): IO[Long] = {
      for {
        amount <- IO.blocking(inStream.read(arr, 0, arr.length))
        _ <-
          if (amount <= -1) IO.pure(acc)
          else IO.blocking(outStream.write(arr, 0, amount)) >> transferInner(arr, acc + amount)
      } yield acc
    }

    transferInner(new Array[Byte](1024 * 10), 0)
  }

  def copy(inFileName: String, outFileName: String): IO[Long] = {
    inputOutputStreams(new File(inFileName), new File(outFileName)).use(io => {
      val inStream = io._1
      val outStream = io._2
      transfer(inStream, outStream)
    })
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- copy("D:\\src\\a.txt", "D:\\src\\b.txt")
    } yield ExitCode.Success
}


object HelloWorld extends IOApp.Simple {
  val run = IO.println("Hello, World!")
}


object StupidFuzzBuzz extends IOApp.Simple {

  import scala.concurrent.duration._

  override def run: IO[Unit] = {
    for {
      ctr <- IO.ref(0)
      wait = IO.sleep(1.second)
      poll = wait *> ctr.get

      _ <- poll.flatMap(IO.println(_)).foreverM.start
      _ <- poll.map(_ % 3 == 0).ifM(IO.println("fizz"), IO.unit).foreverM.start
      _ <- poll.map(_ % 5 == 0).ifM(IO.println("buzz"), IO.unit).foreverM.start

      _ <- (wait *> ctr.update(_ + 1)).foreverM.void

    } yield ()
  }
}




