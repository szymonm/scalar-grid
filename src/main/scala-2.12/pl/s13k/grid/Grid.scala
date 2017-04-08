package pl.s13k.grid

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable}

trait Grid[+V] extends immutable.Map[(Int, Int), V] {
  def row(i: Int): Seq[(Int, V)]
}


class NestedGrid[+V](protected val underlying: immutable.Map[Int, immutable.Map[Int, V]])
  extends Grid[V]
  with immutable.MapLike[(Int, Int), V, NestedGrid[V]]
{
  override def row(i: Int): Seq[(Int, V)] = underlying.get(i).toSeq.flatten

  override def get(key: (Int, Int)): Option[V] = {
    for {
      inner <- underlying.get(key._1)
      res <- inner.get(key._2)
    } yield res
  }

  override def iterator: Iterator[((Int, Int), V)] = underlying.iterator.flatMap{
    case (first, map) => map.iterator.map(x => ((first, x._1),  x._2))
  }

  override def -(key: (Int, Int)): NestedGrid[V] = {
    underlying.get(key._1) match {
      case Some(e) => new NestedGrid(underlying + ((key._1, e - key._2)))
      case None => this
    }
  }

  override def +[V1 >: V](kv: ((Int, Int), V1)): NestedGrid[V1] = {
    underlying.get(kv._1._1) match {
      case Some(e) => new NestedGrid(
        underlying + ((kv._1._1, e + ((kv._1._1, kv._2))))
      )
      case None => new NestedGrid(
        underlying + ((kv._1._1, immutable.Map(kv._1._2 -> kv._2)))
      )
    }
  }


  override def empty: NestedGrid[V] = NestedGrid.empty[V]

  override def updated [V1 >: V](key: (Int, Int), value: V1): NestedGrid[V1] = this + ((key, value))

  override def withDefault[V1 >: V](d: ((Int, Int)) => V1): NestedGrid[V1] = new NestedGrid.WithDefault[V1](this, d)

  override def withDefaultValue[V1 >: V](d: V1): NestedGrid[V1] = new NestedGrid.WithDefault[V1](this, x => d)
}

object NestedGrid {
  def empty[B]: NestedGrid[B] = new NestedGrid[B](immutable.Map[Int, immutable.Map[Int, B]]())

  def newBuilder[B]: mutable.Builder[((Int, Int), B), NestedGrid[B]] =
    new mutable.MapBuilder[(Int, Int), B, NestedGrid[B]](empty)

  implicit def canBuildFrom[B]: CanBuildFrom[NestedGrid[B], ((Int, Int), B), NestedGrid[B]] =
    new CanBuildFrom[NestedGrid[_], ((Int, Int), B), NestedGrid[B]] {
      override def apply(from: NestedGrid[_]): mutable.Builder[((Int, Int), B), NestedGrid[B]] =
        newBuilder[B]
      override def apply(): mutable.Builder[((Int, Int), B), NestedGrid[B]] = newBuilder
    }

  class WithDefault[+V](from: NestedGrid[V], d: ((Int, Int)) => V) extends NestedGrid[V](from.underlying) {
    override def empty = new WithDefault(from.empty, d)
    override def updated[V1 >: V](key: (Int, Int), value: V1): WithDefault[V1] =
      new WithDefault[V1](from.updated[V1](key, value), d)
    override def + [V1 >: V](kv: ((Int, Int), V1)): WithDefault[V1] = updated(kv._1, kv ._2)
    override def - (key: (Int, Int)): WithDefault[V] = new WithDefault(from - key, d)
    override def withDefault[V1 >: V](d: ((Int, Int)) => V1): NestedGrid[V1] = new WithDefault[V1](from, d)
    override def withDefaultValue[V1 >: V](d: V1): NestedGrid[V1] = new WithDefault[V1](from, x => d)
    override def default(x: (Int, Int)) = d(x)
  }
}
