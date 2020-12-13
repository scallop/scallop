package org.rogach.scallop

class OptionGroupsTest extends ScallopTestBase {

  private def assertHelpString(conf: ScallopConf, expectedHelpString: String): Unit = {
    conf.getHelpString() shouldBe expectedHelpString.stripPrefix("\n").stripSuffix("\n")
  }

  test ("single option in a single group with no header") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group()
      val apples = opt[Int]("apples", group = primaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
  -a, --apples  <arg>

  -o, --other  <arg>
  -h, --help            Show help message
""")
  }

  test ("single option in a single group with no header, with trailing arg after") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group()
      val apples = opt[Int]("apples", group = primaryOptions)
      val other = trailArg[Int]("other", required = false)
      verify()
    }
    assertHelpString(Conf, """
  -a, --apples  <arg>

  -h, --help            Show help message

 trailing arguments:
  other (not required)
""")
  }

  test ("single option in a single group with no header, with simple option and trailing arg after") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group()
      val apples = opt[Int]("apples", group = primaryOptions)
      val bananas = opt[Int]("bananas")
      val other = trailArg[Int]("other", required = false)
      verify()
    }
    assertHelpString(Conf, """
  -a, --apples  <arg>

  -b, --bananas  <arg>
  -h, --help             Show help message

 trailing arguments:
  other (not required)
""")
  }

  test ("single option in a single group with header") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>

  -o, --other  <arg>
  -h, --help            Show help message
""")
  }

  test ("two options in a single group with header") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions)
      val bananas = opt[Int]("bananas", group = primaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>
  -b, --bananas  <arg>

  -o, --other  <arg>
  -h, --help             Show help message
""")
  }

  test ("two options added to a group explicitly, in different order") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples")
      val bananas = opt[Int]("bananas")
      primaryOptions.append(bananas, apples)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -b, --bananas  <arg>
  -a, --apples  <arg>

  -o, --other  <arg>
  -h, --help             Show help message
""")
  }

  test ("simple option and props option in a single group") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions)
      val properties = props[Int]('D', group = primaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>
  -Dkey=value [key=value]...

  -o, --other  <arg>
  -h, --help                   Show help message
""")
  }

  test ("properties option added to group via .append") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions)
      val properties = props[Int]('D')
      primaryOptions.append(properties)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>
  -Dkey=value [key=value]...

  -o, --other  <arg>
  -h, --help                   Show help message
""")
  }

  test ("mapped option in a group") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions).map(_ + 1)
      val bananas = opt[Int]("bananas", group = primaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>
  -b, --bananas  <arg>

  -o, --other  <arg>
  -h, --help             Show help message
""")
  }

  test ("mapped option in a group, via .append") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions)
      val bananas = opt[Int]("bananas").map(_ + 1)
      primaryOptions.append(bananas)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>
  -b, --bananas  <arg>

  -o, --other  <arg>
  -h, --help             Show help message
""")
  }

  test ("two option groups without headers") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group()
      val apples = opt[Int]("apples", group = primaryOptions)
      val bananas = opt[Int]("bananas", group = primaryOptions)
      val secondaryOptions = group()
      val count = opt[Int]("count", group = secondaryOptions)
      val weight = opt[BigDecimal]("weight", group = secondaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
  -a, --apples  <arg>
  -b, --bananas  <arg>

  -c, --count  <arg>
  -w, --weight  <arg>

  -o, --other  <arg>
  -h, --help             Show help message
""")
  }

  test ("two option groups with headers") {
    object Conf extends ScallopConf(Nil) {
      val primaryOptions = group("Primary options:")
      val apples = opt[Int]("apples", group = primaryOptions)
      val bananas = opt[Int]("bananas", group = primaryOptions)
      val secondaryOptions = group(header = "Secondary options:")
      val count = opt[Int]("count", group = secondaryOptions)
      val weight = opt[BigDecimal]("weight", group = secondaryOptions)
      val other = opt[Int]("other")
      verify()
    }
    assertHelpString(Conf, """
 Primary options:
  -a, --apples  <arg>
  -b, --bananas  <arg>

 Secondary options:
  -c, --count  <arg>
  -w, --weight  <arg>

  -o, --other  <arg>
  -h, --help             Show help message
""")
  }

  test ("option group in subcommand") {
    object Conf extends ScallopConf(Nil) {
      object cp extends Subcommand("cp") {
        val primaryOptions = group()
        val archive = opt[Boolean]("archive", group = primaryOptions)
        val force = opt[Boolean]("force", group = primaryOptions)
      }
      addSubcommand(cp)
      verify()
    }
    assertHelpString(Conf, """
  -h, --help   Show help message

Subcommand: cp
  -a, --archive
  -f, --force

  -h, --help      Show help message
""")
  }

  test ("two option groups in subcommand") {
    object Conf extends ScallopConf(Nil) {
      object cp extends Subcommand("cp") {
        val primaryOptions = group()
        val archive = opt[Boolean]("archive", group = primaryOptions)
        val force = opt[Boolean]("force", group = primaryOptions)
        val secondaryOptions = group()
        val targetDirectory = opt[String]("target-directory", group = secondaryOptions)
        val context = opt[Boolean]("context", group = secondaryOptions)
      }
      addSubcommand(cp)
      verify()
    }
    assertHelpString(Conf, """
  -h, --help   Show help message

Subcommand: cp
  -a, --archive
  -f, --force

  -t, --target-directory  <arg>
  -c, --context

  -h, --help                      Show help message
""")
  }

  test ("two option groups with headers in subcommand") {
    object Conf extends ScallopConf(Nil) {
      object cp extends Subcommand("cp") {
        val primaryOptions = group("Primary options:")
        val archive = opt[Boolean]("archive", group = primaryOptions)
        val force = opt[Boolean]("force", group = primaryOptions)
        val secondaryOptions = group("Secondary options:")
        val targetDirectory = opt[String]("target-directory", group = secondaryOptions)
        val context = opt[Boolean]("context", group = secondaryOptions)
      }
      addSubcommand(cp)
      verify()
    }
    assertHelpString(Conf, """
  -h, --help   Show help message

Subcommand: cp
 Primary options:
  -a, --archive
  -f, --force

 Secondary options:
  -t, --target-directory  <arg>
  -c, --context

  -h, --help                      Show help message
""")
  }

  test ("option groups in two subcommands") {
    object Conf extends ScallopConf(Nil) {
      object cp extends Subcommand("cp") {
        val primaryOptions = group()
        val archive = opt[Boolean]("archive")
        val force = opt[Boolean]("force")
        primaryOptions.append(archive, force)
      }
      addSubcommand(cp)
      object mv extends Subcommand("mv") {
        val primaryOptions = group()
        val interactive = opt[Boolean]("interactive")
        val update = opt[Boolean]("update")
        primaryOptions.append(interactive, update)
      }
      addSubcommand(mv)
      verify()
    }
    assertHelpString(Conf, """
  -h, --help   Show help message

Subcommand: cp
  -a, --archive
  -f, --force

  -h, --help      Show help message

Subcommand: mv
  -i, --interactive
  -u, --update

  -h, --help          Show help message
""")
  }

}
