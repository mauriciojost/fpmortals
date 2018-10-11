import java.io.FileWriter

import cats.{Applicative, Monad}
import cats.implicits._
import cats.effect.IO

import scala.{Console => ScalaConsole}
import scala.io.Source

object Program {

  trait Logger[F[_]] {
    def log(msg: String): F[Unit]
  }

  trait FileSystem[F[_]] {
    def read(path: String): F[String]
  }

  trait Console[F[_]] {
    def read(): F[String]
    def write(l: String): F[Unit]
  }

  final class FileDumper[F[_]: Monad](FS: FileSystem[F], C: Console[F], L: Logger[F]) {
    def askPathAndDump(): F[Unit] = {
      for {
        _ <- C.write("Enter filename:")
        filename <- C.read()
        _ <- L.log(s"Filename entered: $filename")
        fileContent <- FS.read(filename)
        _ <- C.write(fileContent)
      } yield ()
    }
  }

  final object FileSystemIO extends FileSystem[IO] {
    override def read(path: String): IO[String] = IO(Source.fromFile(path).mkString)
  }

  final object ConsoleIO extends Console[IO] {
    override def read(): IO[String] = IO(ScalaConsole.readLine())
    override def write(l: String): IO[Unit] = IO(println(l))
  }

  final object LoggerIO extends Logger[IO] {
    override def log(msg: String): IO[Unit] = IO{
      val fw = new FileWriter("file.log", true)
      try { fw.write(msg) } finally fw.close()
    }
  }

  def main(args: Array[String]): Unit = {
    val fd = new FileDumper(FileSystemIO, ConsoleIO, LoggerIO)
    fd.askPathAndDump().unsafeRunSync()
  }

}
