package pl.s13k.tuplemap

import org.scalatest.{Matchers, WordSpec}

sealed trait Piece
object Pawn extends Piece
trait Figure extends Piece
object Queen extends Figure
object Bishop extends Figure
object Knight extends Figure


class GridSpec extends WordSpec with Matchers {
  def sampleMap = {
    val map: NestedGrid[Figure] = NestedGrid.empty[Figure]
    map + (((1, 1), Bishop)) + (((2, 2), Knight))
  }
  "Grid" should {
    "contain elements" in {
      val map = sampleMap
      map((1, 1)) should equal (Bishop)
      map.toSeq.sortBy(_._1) should equal (Seq(
        ((1, 1), Bishop),
        ((2, 2), Knight)
      ))
    }

    "support iteration" in {
      val map = sampleMap
      map.iterator.toSeq should equal (Seq(((1, 1), Bishop), ((2, 2), Knight)))
    }

    "support adding superclass" in {
      val map: NestedGrid[Figure] = NestedGrid.empty[Figure]
      (map + (((2, 2), Pawn))) shouldBe an[Grid[Piece]]
    }

    "support row-iteration" in {
      val map = sampleMap
      map.row(1) should equal (Seq((1, Bishop)))
    }


    "map working" in {
      val map: NestedGrid[Figure] = NestedGrid.empty[Figure]
      val map1 = map + (((1, 1), Bishop)) + (((2, 2), Knight))

      map1.map(identity).row(1) should equal (Seq((1, Bishop)))
    }

    "take working" in {
      val map: NestedGrid[Figure] = NestedGrid.empty[Figure]
      val map1 = map + (((1, 1), Bishop)) + (((2, 2), Knight))

      map1.take(4).row(1) should equal (Seq((1, Bishop)))
    }

    "with default" in {
      val map: NestedGrid[Figure] = NestedGrid.empty[Figure]
      val map1 = map + (((1, 1), Bishop)) + (((2, 2), Knight))

      map1.withDefaultValue(Pawn)((3, 3)) should equal (Pawn)
    }

    "with default ++" in {
      val map: NestedGrid[Figure] = NestedGrid.empty[Figure]
      val map1 = map + (((1, 1), Bishop)) + (((2, 2), Knight))

      map1.withDefaultValue(Pawn).row(1) should equal (Seq((1, Bishop)))
    }
  }
}
