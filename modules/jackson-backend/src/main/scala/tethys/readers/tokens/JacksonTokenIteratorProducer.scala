package tethys.readers.tokens

import com.fasterxml.jackson.core.JsonFactory
import tethys.jackson.JacksonTokenIterator
import tethys.readers.{FieldName, ReaderError}

import java.io.Reader

object JacksonTokenIteratorProducer {
  def default(factory: JsonFactory): TokenIteratorProducer = (reader: Reader) =>
    ReaderError.catchNonFatal(JacksonTokenIterator.fromFreshParser(factory.createParser(reader)))(FieldName())
}
