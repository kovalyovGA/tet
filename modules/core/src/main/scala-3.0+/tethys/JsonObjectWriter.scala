package tethys

import tethys.derivation.builder.{Discriminator, FieldStyle, WriterBuilder}
import tethys.derivation.utils.summonAll
import tethys.writers.tokens.TokenWriter
import tethys.{JsonWriter, derivation}

import scala.compiletime.summonInline
import scala.deriving.Mirror

trait JsonObjectWriter[A] extends JsonWriter[A]:
  self =>

  override def write(value: A, tokenWriter: TokenWriter): Unit =
    tokenWriter.writeObjectStart()
    writeValues(value, tokenWriter)
    tokenWriter.writeObjectEnd()


  def writeValues(value: A, tokenWriter: TokenWriter): Unit

  def ++(that: JsonObjectWriter[A]): JsonObjectWriter[A] = concat(that)

  def concat(that: JsonObjectWriter[A]): JsonObjectWriter[A] = (value: A, tokenWriter: TokenWriter) =>
    self.writeValues(value, tokenWriter)
    that.writeValues(value, tokenWriter)

  override def contramap[B](fun: B => A): JsonObjectWriter[B] = (value: B, tokenWriter: TokenWriter) =>
    self.writeValues(fun(value), tokenWriter)


object JsonObjectWriter:
  def apply[A](using writer: JsonObjectWriter[A]): JsonObjectWriter[A] = writer

  inline def derived[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    inline mirror match
      case mirror: Mirror.ProductOf[A]  => writerForProduct(WriterBuilder[A])(using mirror)
      case mirror: Mirror.SumOf[A]      => writerForSum(Discriminator[A])(using mirror)

  inline def derived[A: Mirror.SumOf](inline discriminator: Discriminator[A]): JsonObjectWriter[A] =
    writerForSum[A](discriminator)

  inline def derived[A: Mirror.ProductOf](inline fieldStyle: FieldStyle): JsonObjectWriter[A] =
    writerForProduct(WriterBuilder[A].fieldStyle(fieldStyle))

  inline def derived[A: Mirror.ProductOf](inline builder: WriterBuilder[A]): JsonObjectWriter[A] =
    writerForProduct(builder)

  private inline def writerForProduct[A: Mirror.ProductOf](inline builder: WriterBuilder[A]): JsonObjectWriter[A] =
    new JsonObjectWriter[A]:
      def writeValues(value: A, tokenWriter: TokenWriter) =
        derivation.writer.write(value, tokenWriter, builder)

  private inline def writerForSum[A](inline discriminator: Discriminator[A])(using mirror: Mirror.SumOf[A]): JsonObjectWriter[A] =
    new JsonObjectWriter[A]:
      def writeValues(value: A, tokenWriter: TokenWriter) =
        derivation.writer.writeDiscriminator(value, tokenWriter, discriminator)
        val ord = mirror.ordinal(value)
        summonAll[JsonObjectWriter, mirror.MirroredElemTypes](ord).writeValues(value, tokenWriter)

