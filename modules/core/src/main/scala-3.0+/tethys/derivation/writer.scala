package tethys.derivation

import tethys.derivation.builder.*
import tethys.writers.tokens.TokenWriter
import tethys.{JsonObjectWriter, JsonWriter}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*
import scala.deriving.Mirror


private[tethys] object writer {
  inline def write[A](inline a: A, inline writer: TokenWriter, inline builder: WriterBuilder[A]): Unit =
    ${writeImpl[A]('a, 'writer, 'builder)}

  private def writeImpl[A: Type](value: Expr[A],
                                 tokenWriter: Expr[TokenWriter],
                                 builder: Expr[WriterBuilder[A]])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    val rootTerm = value.asTerm
    val rootRepr = TypeRepr.of[A]
    val nameOfA = rootRepr.show

    sealed trait AbstractField:
      def name: Expr[String]
      def withName(newName: Expr[String]): AbstractField

    case class Field(name: Expr[String], term: Term, tpe: TypeRepr) extends AbstractField :
      override def withName(newName: Expr[String]): Field = copy(name = newName)

    case class PartialField(name: Expr[String], term: Term, tpe: TypeRepr, lambda: Block) extends AbstractField :
      override def withName(newName: Expr[String]): PartialField = copy(name = newName)

    val baseFields = TypeTree.of[A].symbol.caseFields.map(sym =>
      sym.name -> Field(
        Expr(sym.name),
        Select(rootTerm, sym),
        rootRepr.memberType(sym)
      )
    ).toMap

    object Name:
      def unapply(term: Term): Option[Expr[String]] = term match
        case Literal(StringConstant(field)) => Some(Expr(field))
        case _                              =>
          report.errorAndAbort("only string literal is allowed")

    object FieldSelect:
      def unapply(term: Term): Option[String] = term match
        case Block(List(DefDef(_, _, _, Some(Select(Ident(_), name)))), _) =>
          Some(name)
        case _                                                             =>
          report.errorAndAbort(s"only anonymous function from $nameOfA to its members is allowed")

    object BaseUpdate:
      def unapply(term: Term): Option[(String, TypeRepr, Term)] = term match
        case Apply(TypeApply(Select(next, "update"), List(typ1)), List(FieldSelect(field))) =>
          Some((field, typ1.tpe, next))
        case _                                                                              =>
          None

    object BaseUpdateWithRename:
      def unapply(term: Term): Option[(String, Expr[String], TypeRepr, Term)] = term match
        case Apply(Select(BaseUpdate(field, tpe1, next), "withRename"), List(Name(newName))) =>
          Some((field, newName, tpe1, next))
        case _                                                                               =>
          None

    object BasePartialUpdate:
      def unapply(term: Term): Option[(String, TypeRepr, Term)] = term match
        case Apply(TypeApply(Select(next, "updatePartial"), List(typ1)), List(FieldSelect(field))) =>
          Some((field, typ1.tpe, next))
        case _                                                                                     =>
          None

    object BasePartialUpdateWithRename:
      def unapply(term: Term): Option[(String, Expr[String], TypeRepr, Term)] = term match
        case Apply(Select(BasePartialUpdate(field, tpe, next), "withRename"), List(Name(newName))) =>
          Some((field, newName, tpe, next))
        case _                                                                                     =>
          None

    @tailrec
    def updateFields(term: Term,
                     fields: mutable.Map[String, AbstractField],
                     fieldStyle: Option[Expr[FieldStyle]]): (mutable.Map[String, AbstractField], Option[Expr[FieldStyle]]) =
      term match
        case Inlined(_, _, next) =>
          updateFields(next, fields, fieldStyle)

        //WriterBuilder[A]
        case TypeApply(Select(Ident("WriterBuilder"), "apply"), _) =>
          (fields, fieldStyle)

        //.remove(_.field)
        case Apply(TypeApply(Select(next, "remove"), _), List(FieldSelect(field))) =>
          updateFields(next, fields -= field, fieldStyle)

        //.add("name")((a: A) => ???)
        case Apply(TypeApply(Select(Apply(Select(next, "add"), List(Name(name))), "apply"), List(typ)), List(lambda)) =>
          val newField = Field(name, Apply(Select.unique(lambda, "apply"), List(rootTerm)), typ.tpe)

          updateFields(next, fields += (name.valueOrAbort -> newField), fieldStyle)

        //.rename(_.field)("newName")
        case Apply(Apply(TypeApply(Select(next, "rename"), List(typ)), List(FieldSelect(name))), List(Name(newName))) =>
          val newField = baseFields(name).copy(name = newName)
          updateFields(next, fields -= name += newName.valueOrAbort -> newField, fieldStyle)

        //.update[B](_.field)((b: B) => ???)
        case Apply(TypeApply(Select(BaseUpdate(field, tpe1, next), "apply"), List(typ2)), List(lambda)) =>
          val newField = Field(Expr(field), Apply(Select.unique(lambda, "apply"), List(Select.unique(rootTerm, field))), typ2.tpe)

          updateFields(next, fields += (field -> newField), fieldStyle)

        //.update[B](_.field).fromRoot((a: A) => ???)
        case Apply(TypeApply(Select(BaseUpdate(field, _, next), "fromRoot"), List(typ2)), List(lambda)) =>
          val newField = Field(Expr(field), Apply(Select.unique(lambda, "apply"), List(rootTerm)), typ2.tpe)

          updateFields(next, fields += (field -> newField), fieldStyle)

        //.update[B](_.field).withRename("newName")((b: B) => ???)
        case Apply(TypeApply(Select(BaseUpdateWithRename(field, newName, tpe1, next), "apply"), List(typ2)), List(lambda)) =>
          val newField = Field(newName, Apply(Select.unique(lambda, "apply"), List(Select.unique(rootTerm, field))), typ2.tpe)

          updateFields(next, fields -= field += (newName.valueOrAbort -> newField), fieldStyle)

        //.update[B](_.field).withRename("newName").fromRoot((a: A) => ???)
        case Apply(TypeApply(Select(BaseUpdateWithRename(field, newName, _, next), "fromRoot"), List(typ2)), List(lambda)) =>
          val newField = Field(newName, Apply(Select.unique(lambda, "apply"), List(rootTerm)), typ2.tpe)

          updateFields(next, fields -= field += (newName.valueOrAbort -> newField), fieldStyle)

        //.updatePartial[B](_.field) { case ... }
        case Apply(TypeApply(Select(BasePartialUpdate(field, tpe, next), "apply"), _), List(lambda: Block)) =>
          val newField = PartialField(Expr(field), Select.unique(rootTerm, field), tpe, lambda)

          updateFields(next, fields += field -> newField, fieldStyle)

        //.updatePartial[B](_.field).withRename("newName") { case ... }
        case Apply(TypeApply(Select(BasePartialUpdateWithRename(field, newName, tpe, next), "apply"), _), List(lambda: Block)) =>
          val newField = PartialField(newName, Select.unique(rootTerm, field), tpe, lambda)

          updateFields(next, fields -= field += (newName.valueOrAbort -> newField), fieldStyle)

        //.updatePartial[B](_.field).fromRoot { case ... }
        case Apply(TypeApply(Select(BasePartialUpdate(field, _, next), "fromRoot"), _), List(lambda: Block)) =>
          val newField = PartialField(Expr(field), rootTerm, rootRepr, lambda)

          updateFields(next, fields += (field -> newField), fieldStyle)

        //.updatePartial[B](_.field).withRename("newName").fromRoot { case ... }
        case Apply(TypeApply(Select(BasePartialUpdateWithRename(field, newName, _, next), "fromRoot"), _), List(lambda: Block)) =>
          val newField = PartialField(newName, rootTerm, rootRepr, lambda)

          updateFields(next, fields -= field += (newName.valueOrAbort -> newField), fieldStyle)

        //.fieldStyle(...)
        case Apply(Select(next, "fieldStyle"), List(fs)) =>
          updateFields(next, fields, Some(fs.asExprOf[FieldStyle]))

        case term =>
          report.errorAndAbort(s"Unknown tree: ${term.show(using Printer.TreeStructure)}")

    def writeTerm(name: Expr[String], repr: Term, tpe: TypeRepr): Expr[Unit] =
      tpe.widen.asType match
        case '[t] =>
          Expr.summon[JsonWriter[t]] match
            case Some(writer) => '{${writer}.write(${name}, ${repr.asExprOf[t]}, ${tokenWriter})}
            case None         =>
              report.errorAndAbort(s"JsonWriter[${tpe.show}] is missing")

    def applyFieldStyle(fields: mutable.Map[String, AbstractField])(fieldStyle: Expr[FieldStyle]) =
      val oldKeys = fields.keySet.intersect(baseFields.keySet)
      fields.map { case (k, f) =>
        if !oldKeys.contains(k) then
          (k, f)
        else
          (k, f.withName('{${fieldStyle}.applyStyle(${f.name})}))
      }

    val finalFields =
      val (newFields, fs) = updateFields(builder.asTerm, collection.mutable.Map.from(baseFields), None)
      fs.fold(newFields)(applyFieldStyle(newFields))

    def updatePartialFunction[To: Type](pf: Term, andThen: Term => Expr[To]): Block = pf match
      case Block(List(f@DefDef(p1, p2@List(TermParamClause(List(v: ValDef))), p3, Some(m: Match))), closure: Closure) =>
        val newCases = m.cases.map {
          case CaseDef(left, guard, Block(x, res)) => CaseDef(left, guard, Block(x, andThen(res).asTerm))
        }
        val newDef = DefDef.copy(f)(p1, p2, p3, Some(Match(m.scrutinee, newCases)))

        val resultType = v.tpt.tpe.asType match
          case '[from] => TypeRepr.of[PartialFunction[from, To]]

        Block(List(newDef), Closure(closure.meth, Some(resultType)))

      case tree =>
        report.errorAndAbort(s"Not a partial function: ${tree.show(using Printer.TreeShortCode)}")

    finalFields.values.foldLeft('{}) {
      case (acc, Field(name, repr, tpe)) =>
        '{${acc}; ${writeTerm(name, repr, tpe)}}

      case (acc, PartialField(name, term, tpe, pf)) =>
        val updatedPf = updatePartialFunction(pf, t => writeTerm(name, t, t.tpe))
        '{${acc}; ${Apply(Select.unique(updatedPf, "apply"), List(term)).asExprOf[Unit]}}
    }


  inline def writeDiscriminator[A](inline a: A,
                                   inline writer: TokenWriter,
                                   inline discriminator: Discriminator[A]): Unit =
    ${writeDiscriminatorImpl[A]('a, 'writer, 'discriminator)}

  private def writeDiscriminatorImpl[A: Type](value: Expr[A],
                                              tokenWriter: Expr[TokenWriter],
                                              discriminator: Expr[Discriminator[A]])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    val termA = value.asTerm
    val reprA = TypeRepr.of[A]
    val nameOfA = reprA.show

    case class Discrim(label: Expr[String], term: Term, tpe: TypeRepr)

    object Name:
      def unapply(term: Term): Option[Expr[String]] = term match
        case Literal(StringConstant(field)) => Some(Expr(field))
        case _                              =>
          report.errorAndAbort("only string literal is allowed")

    object FieldSelect:
      def unapply(term: Term): Option[String] = term match
        case Block(List(DefDef(_, _, _, Some(Select(Ident(_), name)))), _) =>
          Some(name)
        case _                                                             =>
          report.errorAndAbort(s"only anonymous function from $nameOfA to its members is allowed")

    @tailrec
    def processTree(term: Term, discriminator: Option[Discrim]): Option[Discrim] = term match
      case Inlined(_, _, next) =>
        processTree(next, discriminator)

      case TypeApply(Select(Ident("Discriminator"), "apply"), _) =>
        discriminator

      case Apply(Select(_, "byClass"), List(Name(label))) =>
        val ord = Expr.summon[Mirror.SumOf[A]].get match
          case '{ $m: Mirror.SumOf[A] } => '{ $m.ordinal($value) }
        val children = Expr(TypeTree.of[A].symbol.children.map(_.name))

        Some(Discrim(label, '{ $children($ord) }.asTerm, TypeRepr.of[String]))

      case Apply(TypeApply(Select(_, "by"), List(typ)), List(lambda@FieldSelect(label))) =>
        Some(Discrim(Expr(label), Apply(Select.unique(lambda, "apply"), List(termA)), typ.tpe))

    processTree(discriminator.asTerm, None).fold('{}) {
      case Discrim(label, repr, tpe) =>
        tpe.asType match
          case '[t] =>
            Expr.summon[JsonWriter[t]] match
              case Some(writer) =>
                '{ ${writer}.write(${label}, ${repr.asExprOf[t]}, ${tokenWriter}) }
              case None =>
                report.errorAndAbort(s"JsonWriter[${repr.show}] not found")
    }

}
