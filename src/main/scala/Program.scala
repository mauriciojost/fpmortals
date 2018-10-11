import java.io.FileWriter

import cats.{Applicative, Monad}
import cats.implicits._
import cats.effect.{IO, Sync}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.{Console => ScalaConsole}
import scala.io.Source

object Program {

  // Algebras (their methods represent side effects)
  trait FileSystem[F[_]] {
    def read(path: String): F[String]
  }

  trait Console[F[_]] {
    def read(): F[String]
    def write(l: String): F[Unit]
  }

  // Algebras implementation for a given effect

  final object FileSystemIO extends FileSystem[IO] {
    override def read(path: String): IO[String] = IO(Source.fromFile(path).mkString)
  }

  final object ConsoleIO extends Console[IO] {
    override def read(): IO[String] = IO(ScalaConsole.readLine())
    override def write(l: String): IO[Unit] = IO(println(l))
  }

  // Our business

  final class FileDumper[F[_]: Sync](FS: FileSystem[F], C: Console[F]) {
    def askPathAndDump(): F[Unit] = {
      for {
        logger <- Slf4jLogger.create[F]
        _ <- C.write("Enter filename:")
        filename <- C.read()
        _ <- logger.debug(s"Filename provided: $filename")
        fileContent <- FS.read(filename)
        _ <- logger.debug(s"File $filename read correctly")
        _ <- C.write(fileContent)
        _ <- logger.info(s"File $filename written to console")
      } yield ()
    }
  }

  def main(args: Array[String]): Unit = {
    val fd = new FileDumper(FileSystemIO, ConsoleIO)
    fd.askPathAndDump().unsafeRunSync()
  }

}
