package tethys.compat

import scala.collection.{IterableFactory, IterableFactoryDefaults, MapFactory, mutable}
import scala.quoted.*

trait CollectionBuilder[A, C]:
  def newBuilder: mutable.Builder[A, C]


object CollectionBuilder:

  final class IterableFactoryCollectionBuilder[A, C[_]](factory: IterableFactory[C]) extends CollectionBuilder[A, C[A]]:
    override def newBuilder: mutable.Builder[A, C[A]] = factory.newBuilder[A]


  final class MapFactoryCollectionBuilder[K, V, M[_, _]](factory: MapFactory[M]) extends CollectionBuilder[(K, V), M[K, V]]:
    override def newBuilder: mutable.Builder[(K, V), M[K, V]] = factory.newBuilder[K, V]


  inline implicit def iterableFactoryCollectionBuilder[A, C[X] <: IterableFactoryDefaults[X, C]]: IterableFactoryCollectionBuilder[A, C] =
    ${impl.iterableFactoryCollectionBuilder[A, C]}

  inline implicit def mapFactoryCollectionBuilder[K, V, M[X, Y] <: Map[X, Y]]: MapFactoryCollectionBuilder[K, V, M] =
    ${impl.mapFactoryCollectionBuilder[K, V, M]}


  object impl:
    def iterableFactoryCollectionBuilder[A: Type, C[_]: Type](using Quotes): Expr[IterableFactoryCollectionBuilder[A, C]] =
      import quotes.reflect.*
      val companion = This(TypeRepr.of[C].typeSymbol.companionClass).asExprOf[IterableFactory[C]]
      '{new tethys.compat.CollectionBuilder.IterableFactoryCollectionBuilder[A, C](${companion})}

    def mapFactoryCollectionBuilder[K: Type, V: Type, M[X, Y] <: Map[X, Y]: Type](using Quotes): Expr[MapFactoryCollectionBuilder[K, V, M]] =
      import quotes.reflect.*
      val companion = This(TypeRepr.of[M].typeSymbol.companionClass).asExprOf[MapFactory[M]]
      '{new tethys.compat.CollectionBuilder.MapFactoryCollectionBuilder[K, V, M](${companion})}