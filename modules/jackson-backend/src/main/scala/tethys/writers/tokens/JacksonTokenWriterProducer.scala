package tethys.writers.tokens

import com.fasterxml.jackson.core.JsonFactory
import tethys.jackson.JacksonTokenWriter

import java.io.Writer

object JacksonTokenWriterProducer {
  def default(factory: JsonFactory): TokenWriterProducer = (writer: Writer) =>
    new JacksonTokenWriter(factory.createGenerator(writer))

  def pretty(factory: JsonFactory): TokenWriterProducer = (writer: Writer) =>
    new JacksonTokenWriter(factory.createGenerator(writer).useDefaultPrettyPrinter())
}
