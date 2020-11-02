package com.example.echo

import org.scalatest._
import org.scalatest.flatspec._
import org.http4s.dsl.io._
import cats._
import cats.implicits._
import cats.data._
import cats.effect._
import org.http4s.dsl._
import org.http4s._
import org.http4s.implicits._

class CountTest extends AnyFlatSpec {

  val route = Routes.route[IO].orNotFound

  behavior of "route"

  it should "store and count a value" in (for {
    req <- IO.pure(Request[IO](Method.POST, 
      Uri.unsafeFromString("/input")).withEntity("foo")
    )
    status <- route.run(req).map(_.status)
    _ <- IO.delay(assert(status == Status.Ok))
    req <- IO.pure(Request[IO](Method.GET, 
      Uri.unsafeFromString("/query").withQueryParam("key", "foo"))
    )
    count <- route.run(req).flatMap(_.as[String]).map(Integer.parseInt(_))
    _ <- IO.delay(assert(count == 1))
    req <- IO.pure(Request[IO](Method.GET, 
      Uri.unsafeFromString("/query").withQueryParam("key", "bar"))
    )
    count <- route.run(req).flatMap(_.as[String]).map(Integer.parseInt(_))
    _ <- IO.delay(assert(count == 0))
  } yield ()).unsafeRunSync()
}
