package tethys

import tethys.JsonObjectWriter.writerForSum
import tethys.commons.LowPriorityInstance
import tethys.derivation.builder.*
import tethys.derivation.utils.*
import tethys.writers.instances.AllJsonWriters
import tethys.writers.tokens.TokenWriter
import tethys.writers.instances.SimpleJsonObjectWriter

import scala.collection.{immutable, mutable}
import scala.compiletime.{constValue, summonInline}
import scala.deriving.Mirror

trait JsonWriter[@specialized(specializations) A]:
  self =>

  def write(name: String, value: A, tokenWriter: TokenWriter): Unit =
    tokenWriter.writeFieldName(name)
    write(value, tokenWriter)


  def write(value: A, tokenWriter: TokenWriter): Unit

  def contramap[B](fun: B => A): JsonWriter[B] = (value: B, tokenWriter: TokenWriter) =>
    self.write(fun(value), tokenWriter)


object JsonWriter extends AllJsonWriters:
  def apply[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[A] = jsonWriter

  inline def obj[A]: SimpleJsonObjectWriter[A] = SimpleJsonObjectWriter[A]

  inline def derived[A](using m: Mirror.Of[A]): JsonWriter[A] = JsonObjectWriter.derived[A]

  inline def derived[A: Mirror.ProductOf](inline fieldStyle: FieldStyle): JsonWriter[A] =
    JsonObjectWriter.derived[A](fieldStyle)

  inline def derived[A: Mirror.ProductOf](inline builder: WriterBuilder[A]): JsonWriter[A] =
    JsonObjectWriter.derived[A](builder)

  inline def derivedWith[A: Mirror.SumOf](inline discriminator: Discriminator[A]): JsonWriter[A] =
    JsonObjectWriter.derived[A](discriminator)
