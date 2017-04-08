trait Key
case class DoorKey(id: Int) extends Key
case class EnglishKey(size: Int) extends Key

import scala.collection.immutable.Map

val cabinet = Map[Int, DoorKey](
  1 -> DoorKey(7),
  3 -> DoorKey(8)
)

cabinet + (5 -> EnglishKey(77))
