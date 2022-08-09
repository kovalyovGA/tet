package tethys.derivation

import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}
import tethys.JsonWriter
import tethys.derivation.builder.{FieldStyle, WriterBuilder}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*

private[tethys]
object utils {
  inline def summonAll[C[_], T <: Tuple]: List[C[Any]] =
    inline erasedValue[T] match
      case EmptyTuple   => Nil
      case _: (t *: ts) => summonInline[C[t]].asInstanceOf[C[Any]] :: summonAll[C, ts]

  inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case EmptyTuple   => Nil
      case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabels[ts]


  enum ShowMode:
    case Tree, Code

  inline def show[A](inline a: A, inline mode: ShowMode): Unit = ${showImpl[A]('a, 'mode)}

  private def showImpl[A: Type](a: Expr[A], mode: Expr[ShowMode])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    val printer = mode match
      case '{ShowMode.Tree} => Printer.TreeStructure
      case '{ShowMode.Code} => Printer.TreeShortCode

    '{println(${Expr(a.asTerm.show(using printer))})}

}
