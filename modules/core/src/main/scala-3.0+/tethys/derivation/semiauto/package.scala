package tethys.derivation

import tethys.JsonObjectWriter
import tethys.derivation.builder.{Discriminator, WriterBuilder, WriterDerivationConfig}
import scala.compiletime.summonInline
import scala.deriving.Mirror

package object semiauto:
  @deprecated("Use 'derives tethys.JsonObjectWriter' or 'derives JsonWriter' instead")
  inline def jsonWriter[A: Mirror.Of]: JsonObjectWriter[A] = JsonObjectWriter.derived[A]

  @deprecated("Use 'JsonObjectWriter.derived(WriterBuilder[A]...)' or 'JsonWriter...' instead")
  inline def jsonWriter[A: Mirror.ProductOf](inline builder: WriterBuilder[A]): JsonObjectWriter[A] =
    JsonObjectWriter.derived(builder)

  @deprecated("Use FieldStyle and Discriminator directly")
  inline def jsonWriter[A](inline config: WriterDerivationConfig): JsonObjectWriter[A] =
    inline config match
      case WriterDerivationConfig(Some(fieldStyle), _) =>
        JsonObjectWriter.derived(fieldStyle)(summonInline[Mirror.ProductOf[A]])
      case WriterDerivationConfig(_, Some(label)) =>
        JsonObjectWriter.derived(Discriminator[A].byClass(label))(summonInline[Mirror.SumOf[A]])


  @deprecated("Use WriterBuilder directly")
  inline def describe[A](inline builder: WriterBuilder[A]): WriterBuilder[A] = builder

