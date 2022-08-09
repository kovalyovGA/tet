package tethys.jackson

import com.fasterxml.jackson.core.JsonFactory
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.{JacksonTokenWriterProducer, TokenWriterProducer}

package object pretty {
  implicit def prettyJacksonTokenWriterProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenWriterProducer =
    JacksonTokenWriterProducer.pretty(jsonFactory)

  implicit def jacksonTokenIteratorProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenIteratorProducer =
    tethys.jackson.jacksonTokenIteratorProducer
}
