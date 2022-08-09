package tethys.derivation

import tethys.JsonReader
import tethys.JsonReader.buildProduct
import tethys.derivation.builder.ReaderBuilder
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.quoted.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object reader:


  class StateContainer(var value: String)

  inline def dummy: Any = ${ dummyImpl }

  def dummyImpl(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val stuffDef = '{ val stuff = StateContainer("FoobBar") }
    stuffDef.asTerm match
      case Inlined(_, _, Block(defs, _)) =>
        val stuffDefStat = defs.collectFirst{ case valDef: ValDef => valDef }.get  // This statement defines `val` and needs to be
        // eventually in scope at the site we use `val`
        val stuffRefExpr = Ref(stuffDefStat.symbol).asExprOf[StateContainer]  // This is a reference to `val` via which we can
        // use it later

        val exprThatWorksOnValueItDoesntDefine: Expr[Unit] = '{
          //$stuffRefExpr.value = "Hello World"
          println($stuffRefExpr)
        }

        Block(stuffDefStat :: Nil, exprThatWorksOnValueItDoesntDefine.asTerm).asExpr


  inline def read[A](inline builder: ReaderBuilder[A],
                     it: TokenIterator,
                     fieldName: FieldName): A =
    ${readImpl[A]('builder, 'it, 'fieldName)}

  private def readImpl[A: Type](builder: Expr[ReaderBuilder[A]],
                                it: Expr[TokenIterator],
                                fieldName: Expr[FieldName])(using quotes: Quotes): Expr[A] =
    import quotes.reflect.*
    val typeTreeA = TypeTree.of[A]
    val typeReprA = TypeRepr.of[A]
    val nameOfA = typeReprA.show

    //sealed trait Result
    case class BaseField(name: String, tpe: TypeRepr) //extends Result
    //case class ExtractFieldAs(field: BaseField, lambda: Term, tpe: TypeRepr) extends Result

    val baseFields = typeTreeA.symbol.caseFields
      .map(sym => sym.name -> BaseField(sym.name, typeReprA.memberType(sym)))

    val readers: Expr[List[(String, JsonReader[_])]] = Expr.ofList(baseFields.map((k, f) =>
      f.tpe.asType match
        case '[t] =>
          Expr.summon[JsonReader[t]] match
            case Some(reader) => '{${Expr(k)} -> ${reader}}
            case None         =>
              report.errorAndAbort(s"JsonReader[${f.tpe.show}] not found")
    ))

    val collectValues: Expr[Unit] = '{
      val valueMap = mutable.Map.empty[String, Any]
      val readerMap = ${readers}.toMap
      while(!${it}.currentToken().isObjectEnd) {
        val name = ${it}.fieldName()
        readerMap.get(name) match
          case Some(reader) =>
            valueMap += name -> reader.read(${it}.next())(${fieldName}.appendFieldName(name))
          case _            =>
            ${it}.next()
      }

      val values = ${readers}.map((k, _) => valueMap(k)).toArray
    }

    collectValues.asTerm match
      case Inlined(_, _, Block(defs, _)) =>
        val valDef: Statement = defs.collectFirst { case valDef @ ValDef("values", _, _) => valDef}.get
        val ref = Ref(valDef.symbol).asExprOf[Array[Any]]
        val y = for ((_, f), i) <- baseFields.zipWithIndex.toList
          yield f.tpe.asType match
            case '[t] =>
              '{${ref}.apply(${Expr(i)}).asInstanceOf[t]}.asTerm

        Block(defs, Apply(Select(New(typeTreeA), typeTreeA.symbol.primaryConstructor), y)).asExprOf[A]

/*

    val idx = typeTreeA.symbol.caseFields.zipWithIndex.map((sym, idx) => (idx, sym.name)).toMap

    object FieldSelect:
      def unapply(term: Term): Option[String] = term match
        case Block(List(DefDef(_, _, _, Some(Select(Ident(_), name)))), _) =>
          Some(name)
        case _                                                             =>
          report.errorAndAbort(s"only anonymous function from $nameOfA to its members is allowed")

    object Extract:
      def unapply(term: Term): Option[(BaseField, Term)] = term match
        case Apply(TypeApply(Select(next, "extract"), List(fieldTyp)), List(FieldSelect(fieldName))) =>
          Some((BaseField(fieldName, fieldTyp.tpe), next))
        case _ => None

    val readers: Expr[List[(String, JsonReader[_])]] = Expr.ofList(baseFields.map((k, f) =>
      f.tpe.asType match
        case '[t] =>
          Expr.summon[JsonReader[t]] match
            case Some(reader) =>
              '{${Expr(k)} -> ${reader}}
            case None         =>
              report.errorAndAbort(s"JsonReader[${f.tpe.show}] not found")
    ).toList)
*/
