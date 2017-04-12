package zero.minibatch

import java.util.Random

class Sampler[T](pool: Seq[T]) {
  val r = new Random(System.currentTimeMillis())

  def take(target: Int) = takeImpl(target, (0 until pool.size).toList)

  private def takeImpl(target: Int, given: List[Int]): List[T] = target match {
    case 0 => Nil
    case _ =>
      {
        def takeOnceFrom(choice: List[Int]): (Int, List[Int]) = {
          val rand: Int = (r.nextDouble() * choice.length).toInt
          (choice(rand), (choice take rand) ::: (choice drop (rand + 1)))
        }

        val (takeOnce, rest) = takeOnceFrom(given)
        pool(takeOnce) :: takeImpl(target - 1, rest)
      }
  }
}