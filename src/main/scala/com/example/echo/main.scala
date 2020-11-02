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
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import cats.effect.concurrent._

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Server.stream[IO].compile.drain.as(ExitCode.Success)
}

object Server {
    def stream[F[_]: ConcurrentEffect](implicit T: Timer[F], C: ContextShift[F]): Stream[F, Unit] = for {
      store <- Stream.eval(Ref.of[F, Map[String, Int]](Map[String, Int]()))
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(9000, "0.0.0.0")
        .withHttpApp(Routes.route[F](store).orNotFound)
        .serve
    } yield ()
}

object Routes {

  type Braket[F[_]] = Bracket[F, Throwable]
  object KeyParameter extends QueryParamDecoderMatcher[String]("key")

  def route[F[_] : Sync](storeRef: Ref[F, Map[String, Int]]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] { }
    import dsl._
    
    HttpRoutes.of[F] {
      case req @ POST -> Root / "input" => for {
        key <- req.as[String]
        _ <- storeRef.modify(store => (store.updatedWith(key)({
            case Some(count) => Option(count + 1)
            case _ => Option(1)
          }
        ), ()))
        result <- Ok()
      } yield result
      case GET -> Root / "query" :? KeyParameter(key) => for {
        store <- storeRef.get
        result <- Ok(s"${store.getOrElse(key, 0)}")
      } yield result
    }
  }
}