package org.rogach.scallop

import java.io._
import org.rogach.scallop.tokenize._
import org.rogach.scallop.exceptions.OptionReaderFailure

import scala.collection.{Seq => CSeq}

trait ScallopArgListLoader {

  def loadArgList(args: CSeq[String]): CSeq[String] = {
    args.headOption match {
      case Some(arg) if arg.startsWith("@") =>
        val (filename, inputStream) =
          if (arg == "@--") {
            // read options from stdin
            ("<stdin>", System.in)
          } else {
            // read options from a file (canned config)
            val filename = arg.drop(1)
            (filename, new FileInputStream(filename))
          }
        try {
          val baos = new ByteArrayOutputStream()
          val buffer = new Array[Byte](4096)
          var n = 0
          while ({ n = inputStream.read(buffer); n != -1 }) {
            baos.write(buffer, 0, n)
          }
          ArgumentTokenizer.tokenize(baos.toString()) match {
            case EOF(expected) =>
              throw new OptionReaderFailure(Util.format("Unexpected end of file when reading options from %s, expected %s", filename, expected))
            case Failed =>
              throw new OptionReaderFailure(Util.format("Failed to read options from %s", filename))
            case Matched(tokens, remainingInput) if remainingInput.length == 0 =>
              tokens
            case Matched(_, remainingInput) =>
              throw new OptionReaderFailure(Util.format("Failed to parse all input from %s, remaining input: %s", filename, remainingInput))
          }
        } finally {
          inputStream.close()
        }
      case _ => args
    }
  }

}
