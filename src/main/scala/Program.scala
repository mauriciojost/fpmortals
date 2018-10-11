import cats.{Applicative, Monad}
import cats.implicits._
import cats.effect.IO
import scala.{Console => ScalaConsole}

import scala.io.Source

object Program {

  trait FileSystem[F[_]] {
    def read(path: String): F[String]
  }

  trait Console[F[_]] {
    def read(): F[String]
    def write(l: String): F[Unit]
  }

  final class FileDumper[F[_]: Monad](FS: FileSystem[F], C: Console[F]) {
    def askPathAndDump(): F[Unit] = {
      for {
        _ <- C.write("Enter filename:")
        filename <- C.read()
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

  def main(args: Array[String]): Unit = {
    val fd = new FileDumper(FileSystemIO, ConsoleIO)
    fd.askPathAndDump().unsafeRunSync()
  }

}
