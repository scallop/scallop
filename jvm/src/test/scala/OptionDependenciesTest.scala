package org.rogach.scallop

import org.rogach.scallop.exceptions._

class OptionDependenciesTest extends ScallopTestBase {
  test ("dependsOnAny - success, option is not provided") {
    new ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))

      verify()
    }
  }
  test ("dependsOnAny - failure, option is provided, but dependency is not ") {
    expectException(ValidationFailure("When specifying 'apples', at least one of the following options must be provided: bananas, coconuts")) {
      new ScallopConf(List("-a", "1")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        val coconuts = opt[Int]("coconuts")
        dependsOnAny(apples, List(bananas, coconuts))

        verify()
      }
    }
  }
  test ("dependsOnAny - success, option and dependency are provided") {
    new ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))

      verify()
    }
  }
  test ("dependsOnAny - success, option and two dependencies are provided") {
    new ScallopConf(List("-a","1","-b","2","-c","3")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAny(apples, List(bananas, coconuts))

      verify()
    }
  }

  test ("dependsOnAll - success, option is not provided") {
     new ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAll(apples, List(bananas, coconuts))

      verify()
    }
  }
  test ("dependsOnAll - failure, option is provided, but no dependencies") {
    expectException(ValidationFailure("When specifying 'apples', all of the following options must also be provided: bananas, coconuts")) {
      new ScallopConf(List("-a", "1")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        val coconuts = opt[Int]("coconuts")
        dependsOnAll(apples, List(bananas, coconuts))

        verify()
      }
    }
  }
  test ("dependsOnAll - failure, only one dependency is provided") {
    expectException(ValidationFailure("When specifying 'apples', all of the following options must also be provided: bananas, coconuts")) {
      new ScallopConf(List("-a","1","-b","2")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        val coconuts = opt[Int]("coconuts")
        dependsOnAll(apples, List(bananas, coconuts))

        verify()
      }
    }
  }

  test ("dependsOnAll - success, option and two dependencies are provided") {
    new ScallopConf(List("-a","1","-b","2","-c","3")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      dependsOnAll(apples, List(bananas, coconuts))

      verify()
    }
  }

  test ("conflicts - success, no options are provided") {
    new ScallopConf(List()){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      val coconuts = opt[Int]("coconuts")
      conflicts(apples, List(bananas, coconuts))

      verify()
    }
  }

  test ("conflicts - success, only one option is provided") {
    new ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      conflicts(apples, List(bananas))

      verify()
    }
  }

  test ("conflicts - failure, both options are provided") {
    expectException(ValidationFailure("Option 'apples' conflicts with option 'bananas'")) {
      new ScallopConf(List("-a","1","-b","2")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        conflicts(apples, List(bananas))

        verify()
      }
    }
  }

  test ("requireAtLeastOne - failure, nothing is provided") {
    expectException(ValidationFailure("There should be at least one of the following options: apples, bananas")) {
      new ScallopConf(List()){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        requireAtLeastOne(apples, bananas)

        verify()
      }
    }
  }

  test ("requireAtLeastOne - success, one is provided") {
    new ScallopConf(List("-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      requireAtLeastOne(apples, bananas)

      verify()
    }
  }

  test ("requireAtLeastOne - success, all are provided") {
    new ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      requireAtLeastOne(apples, bananas)

      verify()
    }
  }

  test ("requireOne - failure, no options are provided") {
    expectException(ValidationFailure("There should be exactly one of the following options: apples, bananas")) {
      new ScallopConf(List()){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        requireOne(apples, bananas)

        verify()
      }
    }
  }

  test ("requireOne - success, one is provided") {
    new ScallopConf(List("-a", "1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      requireOne(apples, bananas)

      verify()
    }
  }

  test ("requireOne - failure, both options are provided") {
    expectException(ValidationFailure("There should be exactly one of the following options: apples, bananas")) {
      new ScallopConf(List("-a","1","-b","2")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        requireOne(apples, bananas)

        verify()
      }
    }
  }

  test ("option set validation, mutually exclusive options, success") {
    new ScallopConf(List("-a","1")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      mutuallyExclusive(apples, bananas)

      verify()
    }
  }

  test ("option set validation, mutually exclusive options, failure") {
    expectException(ValidationFailure("There should be only one or zero of the following options: apples, bananas")) {
      new ScallopConf(List("-a", "1", "-b", "2")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        mutuallyExclusive(apples, bananas)

        verify()
      }
    }
  }

  test ("option set validation, codependent options, success") {
    new ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      codependent(apples, bananas)

      verify()
    }
  }

  test ("option set validation, codependent options, failure") {
    expectException(ValidationFailure("Either all or none of the following options should be supplied, because they are co-dependent: apples, bananas")) {
      new ScallopConf(List("-a", "1")){
        val apples = opt[Int]("apples")
        val bananas = opt[Int]("bananas")
        codependent(apples, bananas)

        verify()
      }
    }
  }

  test ("option set validation, all defined or not defined, no default values, success") {
    new ScallopConf(List("-a","1","-b","2")){
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      allDefinedOrUndefined(apples, bananas)

      verify()
    }
  }

  test ("option set validation, all defined or not defined, one default value, success") {
    new ScallopConf(List("-b","2")){
      val apples = opt[Int]("apples", default = Some(1))
      val bananas = opt[Int]("bananas")
      allDefinedOrUndefined(apples, bananas)

      verify()
    }
  }

  test ("option set validation, all defined or not defined, both default values, success") {
    new ScallopConf(List()){
      val apples = opt[Int]("apples", default = Some(1))
      val bananas = opt[Int]("bananas", default = Some(2))
      allDefinedOrUndefined(apples, bananas)

      verify()
    }
  }

  test ("option set validation, all defined or not defined, one default value, failure") {
    expectException(ValidationFailure("Either all or none of the following options should be defined, because they are co-dependent: apples, bananas")) {
      new ScallopConf(List()){
        val apples = opt[Int]("apples", default = Some(1))
        val bananas = opt[Int]("bananas")
        allDefinedOrUndefined(apples, bananas)

        verify()
      }
    }
  }

  test ("mutually exclusive flag options - validation success") {
    new ScallopConf(List("-a")) {
      val apples = opt[Boolean]("apples")
      val bananas = opt[Boolean]("bananas")
      mutuallyExclusive(apples,bananas)

      verify()
    }
  }

  test ("mutually exclusive flag options - validation failure") {
    expectException(ValidationFailure("There should be only one or zero of the following options: apples, bananas")) {
      new ScallopConf(List("-a", "-b")) {
        val apples = opt[Boolean]("apples")
        val bananas = opt[Boolean]("bananas")
        mutuallyExclusive(apples,bananas)

        verify()
      }
    }
  }

  test ("property dependsOnAny property - validation success") {
    new ScallopConf(List()) {
      val propsA = props[String]('A')
      val propsB = props[String]('B')
      dependsOnAny(propsA, List(propsB))
      verify()
    }
  }

  test ("property dependsOnAny property - validation failure") {
    expectException(ValidationFailure("When specifying 'A', at least one of the following options must be provided: B")) {
      new ScallopConf(List("-Ay=x")) {
        val propsA = props[String]('A')
        val propsB = props[String]('B')
        dependsOnAny(propsA, List(propsB))
        verify()
      }
    }
  }

  test ("option dependsOnAny property - validation success") {
    new ScallopConf(List()) {
      val apples = opt[Boolean]("apples")
      val propsB = props[String]('B')
      dependsOnAny(apples, List(propsB))
      verify()
    }
  }

  test ("option dependsOnAny property - validation failure") {
    expectException(ValidationFailure("When specifying 'apples', at least one of the following options must be provided: B")) {
      new ScallopConf(List("-a")) {
        val apples = opt[Boolean]("apples")
        val propsB = props[String]('B')
        dependsOnAny(apples, List(propsB))
        verify()
      }
    }
  }

  test ("property conflicts property - validation success") {
    new ScallopConf(List("-Ay=x")) {
      val propsA = props[String]('A')
      val propsB = props[String]('B')
      conflicts(propsA, List(propsB))
      verify()
    }
  }

  test ("property conflicts property - validation failure") {
    expectException(ValidationFailure("Option 'A' conflicts with option 'B'")) {
      new ScallopConf(List("-Ay=x", "-Ba=1")) {
        val propsA = props[String]('A')
        val propsB = props[String]('B')
        conflicts(propsA, List(propsB))
        verify()
      }
    }
  }
}
