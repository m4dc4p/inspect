# Setup (Scal)

To run this application, make sure you have Java installed (at least JDK 1.8/Java 8).

Then, download & install SBT (https://scala-sbt.org)

Once `sbt` is installed, run it in this directory:

```bash
$ sbt
```

At the prompt, enter the `run` command, which will start a server on port 9000:

```bash
sbt> run
[info] running com.example.echo.Main
23:32:25.857 [ioapp-compute-0] INFO org.http4s.blaze.channel.nio1.NIO1SocketServerGroup - Service bound to address /0:0:0:0:0:0:0:0:9000
23:32:25.866 [blaze-selector-0] DEBUG org.http4s.blaze.channel.nio1.SelectorLoop - Channel initialized.
23:32:25.870 [ioapp-compute-0] INFO org.http4s.server.blaze.BlazeServerBuilder -
  _   _   _        _ _
 | |_| |_| |_ _ __| | | ___
 | ' \  _|  _| '_ \_  _(_-<
 |_||_\__|\__| .__/ |_|/__/
             |_|
23:32:25.938 [ioapp-compute-0] INFO org.http4s.server.blaze.BlazeServerBuilder - http4s v1.0.0-M5 on blaze v0.14.13 started at http://[::]:9000/
```

# Setup (Haskell)

Use cabal to run the sample application:

```bash
$ cabal run
```