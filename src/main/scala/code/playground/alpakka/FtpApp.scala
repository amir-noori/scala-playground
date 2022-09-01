package code.playground.alpakka

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.alpakka.ftp.scaladsl.Sftp
import akka.stream.alpakka.ftp.{FtpCredentials, FtpFile, SftpSettings}
import akka.stream.scaladsl.{FileIO, Sink}

import java.net.InetAddress
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object FtpApp extends App {

  implicit val actorSystem: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "ftp-actor")
  implicit val executionContext: ExecutionContext = actorSystem.executionContext


  val ftpSettings = SftpSettings(InetAddress.getByName("10.19.70.77"))
    .withCredentials(FtpCredentials.create("business", "busi123"))
    .withStrictHostKeyChecking(false)

  println("connecting to FTP server...")
  //  val fetchedFiles: Future[immutable.Seq[(String, IOResult)]] = Sftp.ls("/home/business", ftpSettings)
  //    .filter(file => file.isFile)
  //    .mapAsync(2)(file => {
  //      println(s"remote file path: ${file.path}")
  //
  //      val destinationPath = "D:\\" + file.name
  //      val fetchedFile = Sftp.fromPath(file.path, ftpSettings)
  //        .runWith(FileIO.toPath(Path.of(destinationPath)))
  //
  //      fetchedFile.map(ioResult => {
  //        (destinationPath, ioResult)
  //      })
  //    }).runWith(Sink.seq)

  val fetchedFiles: Future[Seq[FtpFile]] =
    Sftp
      .ls("/home/business/test", ftpSettings)
      .filter(file => file.isFile).collect { case f if f.isFile => f }
      .runWith(Sink.seq)

  fetchedFiles
    .onComplete {
      case Success(values) if values.isEmpty =>
        println("there are no files in the target FTP directory.")
      case Success(values) if values.nonEmpty =>
        println("all files fetched.")
        values.foreach(f => {
          val fileName = f.name
          println(s"file name: ${fileName} - file size: ${f.size}")
          if (f.size > 0) {
            val destination = "D:\\" + fileName
            Sftp
              .fromPath(f.path, ftpSettings)
              .runWith(FileIO.toPath(Path.of(destination)))
          }
          actorSystem.terminate()
        })
      case Success(errors) =>
        println(s"errors occurred: ${errors.mkString("\n")}")
      case Failure(exception) =>
        println(s"the stream failed $exception")
    }

}
