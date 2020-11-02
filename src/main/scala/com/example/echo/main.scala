package com.example.echo

import cats._
import cats.implicits._
import cats.data._
import cats.effect._
import org.http4s.dsl._
import fs2.Stream
import org.http4s._
import org.http4s.implicits._
import scala.concurrent.ExecutionContext.global
import org.http4s.server.blaze.BlazeServerBuilder
import scala.collection.concurrent
import org.http4s.dsl.impl.QueryParamDecoderMatcher

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Server.stream[IO].compile.drain.as(ExitCode.Success)
}

object Server {
    def stream[F[_]: ConcurrentEffect](implicit T: Timer[F], C: ContextShift[F]): Stream[F, ExitCode] = for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(9000, "0.0.0.0")
        .withHttpApp(Routes.route[F].orNotFound)
        .serve
    } yield exitCode
}

object Routes {

  object KeyParameter extends QueryParamDecoderMatcher[String]("key")

  val store: concurrent.Map[String, Int] = concurrent.TrieMap[String, Int]()

  def route[F[_] : Sync]: HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] { }
    import dsl._
    
    HttpRoutes.of[F] {
      case req @ POST -> Root / "input" => for {
        key <- req.as[String]
        _ <- Sync[F].delay({
          store.updateWith(key)({
            case Some(count) => Option(count + 1)
            case _ => Option(1)
          })
        })
        result <- Ok()
      } yield result
      case GET -> Root / "query" :? KeyParameter(key) => for {
        count <- Sync[F].delay(store.get(key).getOrElse(0))
        result <- Ok(s"${count}")
      } yield result
    }
  }
}