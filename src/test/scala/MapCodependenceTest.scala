
package org.rogach.scallop

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

class MapCodependenceTest extends FunSuite {

	case object Err extends Exception

	class TestConf(args: Seq[String]) extends ScallopConf(args) {
		val apples = opt[Boolean]("apples")
		val bananas = opt[Map[String,String]]("bananas")
		codependent(apples, bananas)
		errorMessageHandler = error => throw Err
	}
	
	test("failing codependency including unsupplied map") {
		intercept[Err.type] {
			new TestConf(Seq("--apples"))
		}
	}

	test("succeeding codependency including supplied map") {
		new TestConf(Seq("--apples", "--bananas", "zoop=zop"))
	}

	test("succeeding codependency including unsupplied map") {
		new TestConf(Seq.empty)
	}

}

