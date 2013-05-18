include(macro.m4)
New changes since version `0.7.0`:

* Default values of simple options and trailing args are now lazily evaluated (I(38))
* Now `ScallopConf.props` and `ScallopConf.propsLong`
  return a `Map` instead of `(String => Option[A])` (I(39))
* Improvements in help output: `--help` and `--version` are now always included in output,
  and developer can specify what options should go to the top of the list
  (via mainOptions on scaladoc(ScallopConf)) (I(46))
* Also, help output is now clear of trailing whitespace
* Improved option name guessing - now works with properties and trailing arguments
  (I(47), thanks to githubUser(<Christopher Hodapp>,<clhodapp>))
* New option type: "tally". Counts the amount of time the option was invoked,
  useful for setting verbosity level (like `-vvv`) (I(50))
* Added `flatMap` on scaladoc(ValueConverter) (I(53))
* Added `validateOpt` on scaladoc(ScallopConf) to enable more flexible validation of arguments (I(55))
* Bugfixes (I(40), I(51), I(57), I(59), I(60))
