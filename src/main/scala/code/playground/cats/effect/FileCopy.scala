package code.playground.cats.effect

import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import java.io.{File, FileInputStream, FileOutputStream}

object FileCopy {

  def copyFile(origin: File, destination: File): IO[Unit] = {
    ioStream(origin, destination).use {
      case (i, o) => copy(i, o)
    }
  }

  def copy(src: FileInputStream, des: FileOutputStream): IO[Unit] = {
    val buffer: Array[Byte] = new Array[Byte](1024 * 10)
    for {
      _ <- IO.blocking(src.read(buffer))
      _ <- IO.blocking(des.write(buffer))
    } yield IO.unit
  }

  def ioStream(src: File, des: File): Resource[IO, (FileInputStream, FileOutputStream)] =
    for {
      fis <- fileInputStream(src)
      fos <- fileOutputStream(des)
    } yield Tuple2(fis, fos)

  def fileInputStream(file: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(file))
    } { inputStream =>
      IO.blocking(inputStream.close()).handleErrorWith(_ => IO.unit)
    }

  def fileOutputStream(file: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(file))
    } { outputStream =>
      IO.blocking(outputStream.close()).handleErrorWith(_ => IO.unit)
    }

}

object FileCopyApp extends IOApp {

  import FileCopy._

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- copyFile(new File("D:\\x"), new File("D:\\y"))
    } yield ExitCode.Success
  }
}
