import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.lang.{System, SecurityManager, SecurityException}
import java.security.Permission

class StrangeTest extends FunSuite with ShouldMatchers {
  
  /** Captures all output from the *fn* block into two strings - (stdout, stderr). */
  def captureOutput(fn: => Unit):(String,String) = {
    val normalOut = Console.out
    val normalErr = Console.err
    val streamOut = new ByteArrayOutputStream()
    val streamErr = new ByteArrayOutputStream()
    Console.setOut(streamOut)
    Console.setErr(streamErr)
    fn
    Console.setOut(normalOut)
    Console.setErr(normalErr)
    (streamOut.toString, streamErr.toString)
  }
  
  /** Supresses exit in *fn* block. Returns list of exit statuses that were attempted. */
  def trapExit(fn: => Unit):List[Int] = {
    @volatile var statuses = List[Int]()
    val normalSM = System.getSecurityManager
    object SM extends SecurityManager {
      override def checkExit(status:Int) {
        statuses ::= status
        throw new SecurityException
      }
      override def checkPermission(p:Permission) {}
    }
    System.setSecurityManager(SM)
    try {
      fn
    } catch { 
      case e:SecurityException => 
    }
    System.setSecurityManager(normalSM)
    statuses.reverse
  }

  /** Runs program with needed input. */
  def withInput[A](input:String)(fn: => A):A = {
    val normalIn = System.in
    val streamIn = new ByteArrayInputStream(input.getBytes)
    System.setIn(streamIn)
    val res = fn
    System.setIn(normalIn)
    res
  }
  
  test("help printing") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--help")) {
          version("0.1.2")
          banner("some rubbish")
          footer("and some more")
          val apples = opt[Int]("apples", descr = "fresh apples!")
          val verbose = toggle("verbose", descrYes = "very verbose", descrNo = "turn off")
          val trail = trailArg[String]("trail", descr = "Which trail do you choose?")
          verify
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |some rubbish
                        |  -a, --apples  <arg>
                        |    fresh apples!
                        |  --noverbose
                        |    turn off 
                        |  -v, --verbose
                        |    very verbose 
                        |
                        |Trailing arguments:
                        |  trail (required)
                        |    Which trail do you choose?
                        |
                        |and some more
                        |""".stripMargin)
  }
  
  test ("version printing") {
    val (out, err) = captureOutput {
      val exits = trapExit {
        object Conf extends ScallopConf(List("--version")) {
          version("0.1.2")
          val apples = opt[Int]("apples")
          verify
        }
        Conf
      }
      exits.size should equal (1)
    }
    out should equal ("""0.1.2
                        |""".stripMargin)
  }
  
  test ("reading options from stdin") {
    withInput("-a 3\n-b 5") {
      object Conf extends ScallopConf(List("@--")) {
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        verify
      }
      Conf.apples() should equal (3)
      Conf.bananas() should equal (5)
    }
  }
  
  test ("reading options from file") {
    object Conf extends ScallopConf(List("@src/test/resources/opts.txt")) {
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      verify
    }
    Conf.apples() should equal (3)
    Conf.bananas() should equal (5)
  }
  
}
