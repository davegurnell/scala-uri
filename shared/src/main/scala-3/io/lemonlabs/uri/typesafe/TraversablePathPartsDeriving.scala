package io.lemonlabs.uri.typesafe

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

trait TraversablePathPartsDeriving {
  self: TraversablePathPartsConstructors =>

  inline def product[A](implicit m: Mirror.ProductOf[A]): TraversablePathParts[A] = {
    val elemInstances = summonAll[m.MirroredElemTypes]

    instance { (a: A) =>
      a.asInstanceOf[Product]
        .productIterator
        .zip(elemInstances)
        .flatMap { case (field, tc) => tc.asInstanceOf[TraversablePathParts[Any]].toSeq(field) }
        .toSeq
    }
  }

  inline def summonAll[T <: Tuple]: List[TraversablePathParts[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[TraversablePathParts[t]] :: summonAll[ts]
    }
}
