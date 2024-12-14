package typology

import scala.annotation.implicitNotFound
import scala.compiletime.constValueTuple
import scala.deriving.Mirror
import scala.NamedTuple.{AnyNamedTuple, NamedTuple}

/** Operations related to Tuples/NamedTuples */
object Tuplelogy:
  /** Intersection[(A, B, C)] =>> A & B & C */
  type Intersection[A <: Tuple] = Tuple.Fold[A, Any, [a, b] =>> a & b]

  /** Extract the names of a NamedTuple */
  type NamesOf[T] = T match { case NamedTuple[n, _] => n }

  /** Extract the value types of a NamedTuple */
  type ValuesOf[T] = T match { case NamedTuple[_, v] => v }

  /** A compile-time-only name-value tuple.
    *
    * This class isn't instantiated, it's just used as a type with the desired
    * subtyping relationship.
    */
  final class NameValue[N, +V]

  /** Returns name-value type pairs for the given `Names` and `Types` */
  type NameValues[Names <: Tuple, Types <: Tuple] =
    Tuple.Map[
      Tuple.Zip[Names, Types],
      [x] =>> x match { case (name, tpe) => NameValue[name, tpe] }
    ]

  extension [N <: Tuple, V <: Tuple](source: NamedTuple[N, V])
    inline def mapByName[Target <: AnyNamedTuple](using
        SourceN: Mirror.ProductOf[N],
        SourceV: Mirror.ProductOf[V],
        @implicitNotFound("mapByName: [Target] must be a named tuple: ${Target}") TargetN: Mirror.ProductOf[NamesOf[Target]],
        @implicitNotFound("mapByName: [Target] must be a named tuple: ${Target}") TargetV: Mirror.ProductOf[ValuesOf[Target]]
    )(using
        @implicitNotFound("Cannot mapByName from NamedTuple[${N}, ${V}] to ${Target}")
        ev:  Intersection[NameValues[SourceN.MirroredElemTypes, SourceV.MirroredElemTypes]] 
        <:<  Intersection[NameValues[TargetN.MirroredElemTypes, TargetV.MirroredElemTypes]]
    ): Target =
      val sourceValuesByIndex =
        constValueTuple[SourceV.MirroredElemLabels].toArray
          .zip(source.toArray)
          .toMap
          .asInstanceOf[scala.collection.Map[Any, Any]]

      val sourceNamesToIndex = constValueTuple[SourceN.MirroredElemTypes].toArray
        .zip(constValueTuple[SourceN.MirroredElemLabels].toArray: Array[AnyRef])
        .toMap
        .asInstanceOf[scala.collection.Map[Any, Any]]

      val targetLabels = constValueTuple[TargetN.MirroredElemTypes]

      val values = targetLabels.productIterator
        .map(sourceNamesToIndex)
        .map(sourceValuesByIndex)
        .toArray

      Tuple.fromArray[Any](values).asInstanceOf[Target]
    end mapByName
