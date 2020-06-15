package philosopher

import zio._
import zio.clock.Clock
import zio.console.{Console, putStrLn}
import zio.duration._
import zio.stm.{STM, TRef}

object Philosopher extends App {

  override def run(
    args: List[String]
  ): ZIO[Console with Clock, Nothing, ExitCode] = {

    val philosophers = 3
    val meals = 2

    object Chopstick extends Enumeration {
      val Up = Value("up")
      val Down = Value("down")
    }

    case class Philosopher(id: Int,
                           meals: Ref[Int],
                           left: TRef[Chopstick.Value],
                           right: TRef[Chopstick.Value])

    object Philosopher {

      private def toggle(left: TRef[Chopstick.Value],
                         right: TRef[Chopstick.Value],
                         from: Chopstick.Value,
                         to: Chopstick.Value): IO[Nothing, Unit] =
        STM.atomically {
          for {
            _ <- left.get.retryUntil(_ == from)
            _ <- right.get.retryUntil(_ == from)
            _ <- left.set(to)
            _ <- right.set(to)
          } yield ()
        }

      def pickUp(left: TRef[Chopstick.Value],
                 right: TRef[Chopstick.Value]): IO[Nothing, Unit] =
        toggle(left, right, Chopstick.Down, Chopstick.Up)

      def putDown(left: TRef[Chopstick.Value],
                  right: TRef[Chopstick.Value]): IO[Nothing, Unit] =
        toggle(left, right, Chopstick.Up, Chopstick.Down)
    }

    def rounds(
      philosopher: Philosopher
    ): ZIO[Console with Clock, Nothing, Int] = {
      val r = for {
        _ <- putStrLn(s"${philosopher.id} is thinking")
        _ <- ZIO.sleep(1.second)
        _ <- putStrLn(s"${philosopher.id} tries to pick a par of chopstick")
        _ <- Philosopher.pickUp(philosopher.left, philosopher.right)
        _ <- putStrLn(s"${philosopher.id} picked a par of chopstick")
        _ <- putStrLn(s"${philosopher.id} is eating the meal")
        _ <- ZIO.sleep(1.second)
        _ <- putStrLn(s"${philosopher.id} ate the meal")
        _ <- putStrLn(s"${philosopher.id} is putting down the chopsticks")
        _ <- Philosopher.putDown(philosopher.left, philosopher.right)
        _ <- putStrLn(s"${philosopher.id} put down the chopsticks")
        meals <- philosopher.meals.updateAndGet(_ - 1)
        _ <- putStrLn(s"${philosopher.id} has now ${meals} meal(s) left")
      } yield meals

      r.repeat(Schedule.doWhile[Int](_ > 0))
    }

    val program = for {
      chopsticks <- ZIO.collectAll {
        List.fill(philosophers)(TRef.makeCommit(Chopstick.Down))
      }
      meals <- ZIO.collectAll {
        List.fill(philosophers)(Ref.make(meals))
      }
      table = {
        (chopsticks :+ chopsticks.head)
          .sliding(2)
          .toList
          .zip(meals)
          .zipWithIndex
          .map {
            case ((left :: right :: _, meals), id) =>
              Philosopher(id = id, meals = meals, left = left, right = right)
          }
      }
      fork <- ZIO.forkAll {
        table.map(rounds)
      }
      _ <- fork.join
    } yield ()

    program.exitCode
  }
}
