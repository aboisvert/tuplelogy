package typology

import scala.annotation.implicitNotFound
import scala.compiletime.{constValue, constValueTuple}
import scala.deriving.Mirror
import scala.NamedTuple.{AnyNamedTuple, NamedTuple, build}

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

  /** Value type for the field labelled `L` in `NamedTuple[N, V]`. */
  type FieldType[N <: Tuple, V <: Tuple, L <: String & Singleton] =
    (N, V) match
      case (L *: ntail, vh *: vtail)  => vh
      case (nh *: ntail, vh *: vtail) => FieldType[ntail, vtail, L]

  extension [N <: Tuple, V <: Tuple](source: NamedTuple[N, V])

    /** Access the first field by default. */
    transparent inline def ^ : Tuple.Head[V] = source.apply(0)
    transparent inline def > : Tuple.Head[V] = source.apply(0)

    /** Stage a field label for appending; continue with `:=` and the value.
      *
      * Example: `(name = "Alex", age = 42)` `with` `["city"] := "Chicago"`
      */
    transparent inline def update[L <: String & Singleton, A](name: L, value: A)(using
      @implicitNotFound(
        "`update`(…name…) = …value…: cannot update — that name is already present in this named tuple"
      )
      ev: Tuple.Disjoint[N, L *: EmptyTuple] =:= true
    ):
      NamedTuple.Concat[NamedTuple[N, V], NamedTuple[L *: EmptyTuple, A *: EmptyTuple]] =
           source ++ build[L *: EmptyTuple]()(value *: EmptyTuple)

    /** Replace the value at an existing label `name`.
      *
      * Example: `(name = "Alex", age = 42).update_!("age")(99)`
      */
    transparent inline def update_![L <: String & Singleton](name: L)(using
        @implicitNotFound(
          "`update_!`(…name…) = …value…: that name is not present in this named tuple"
        )
        ev: Tuple.Disjoint[N, L *: EmptyTuple] =:= false,
        mn: Mirror.ProductOf[N],
        mv: Mirror.ProductOf[V]
    )(value: FieldType[N, V, L]): NamedTuple[N, V] =
      val wanted = constValue[L]
      val nameToValueLabel = constValueTuple[mn.MirroredElemTypes]
        .productIterator
        .map(_.asInstanceOf[AnyRef])
        .zip(constValueTuple[mn.MirroredElemLabels].toArray.iterator)
        .toMap
      val valueLabel = nameToValueLabel(wanted)
      val valueLabels = constValueTuple[mv.MirroredElemLabels].toArray
      val idx = valueLabels.indexWhere(_ == valueLabel)
      val arr = source.toArray.asInstanceOf[Array[Any]]
      arr(idx) = value.asInstanceOf[Any]
      Tuple.fromArray(arr).asInstanceOf[NamedTuple[N, V]]
    end update_!

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
