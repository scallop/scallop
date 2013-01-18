include(macro.m4)
New changes since version `0.5.1`:

* Now cross-building for Scala 2.9.x and 2.10.
* Option name guessing - now you can write `val apples = opt[Int]()` instead of `val apples = opt[Int]("apples")`. 
  CamelCase field names are converted to hyphen-case option names.
* Added .printHelp method on scaladoc(ScallopConf).
* Improved flexibility in controlling error printing in scaladoc(ScallopConf) (`printedName` & `errorMessageHandler`).
* Short listing of subcommands in main help (activated using `shortSubcommandHelp()`), and specific help for each subcommand. (I(36))
* Several bugfixes - I(23), I(26), I(27), I(31), I(33), I(34), and others.
