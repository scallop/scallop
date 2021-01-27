# Upcoming release

## Features
## Bugfixes
## Other

# v4.0.2
- fix: do not throw "excess arguments" error if there is only one multi-arg option (#224);
- upgrade Scala Native dependency to 0.4, see #225 (@vhiairrassary);
- handle single and double quoted strings when reading options from file in Scala Native;

# v4.0.1
- fix: make ScallopConf.builder accessible again;

# v4.0.0
- BREAKING: `singleArgConverter` and `listArgConverter` now include exception text if provided handler didn't catch the exception;
- BREAKING: `singleArgConverter2` was removed;
- BREAKING: `ArgType.V` is now sealed abstract class;
- BREAKING: `ScallopHelpFormatter.getOptionLines` (and similar methods) now return `List[Either[String, CliOption]]` instead of `List[Option[CliOption]]`;
- BREAKING: behavior change: Scallop now supports trailing arguments before options, thus `--opt1 optArg1 trailArg1 --opt2` will now be parsed instead of throwing an error;
- BREAKING: behavior change: Scallop now handles single and double quotes in a shell-like fashion when reading options from stdin or file;
- added support for trailing arguments before (or between) options, see #147;
- added cross-compilation for Scala 3.0.0-M2, see #215 and #216 (@Sciss);
- added support for option ordering in help output via option groups, see #196;
- handle single and double quoted strings when reading options from stdin or file, allows for using arguments with spaces in such cases;

# v3.5.1
- added path list converter, see #212 (@danielyli);

# v3.5.0
- made the default value for the noshort parameter configurable, see #206 and #207 (@zawlazaw);

# v3.4.0
- added Duration converters, see #199 and #200 (@2m)

# v3.3.2
- allowed requireSubCommand() for nested subcommands, see #197;

# v3.3.1
- added support for Scala 2.13, see #190 (@Philippus);
- fixed copyArrayToImmutableIndexedSeq warning, see #191 (@Sciss);

# v3.3.0
- added cross-compilation for Scala 2.13.0-RC2, see #183, #184 and #186 (@Philippus);
- added support for negative numbers in trailing args, see #189;

# v3.2.0
- added Path validators, see #182 (@rvanheest);

# v3.1.5
- fixed isSupplied on transformed options with guessed names, see #178;
- do not print version info if option with implicit short name -v is supplied for subcommand, see #177;

# v3.1.4
- consider implicit guessed option short names when evaluating -h and -v args, see #177;

# v3.1.3
- fixed short names for toggle options, see #159 (@jairamc);
- fixed propsLong help formatting, see #172;
- fixed scala 2.13.0-M4 compilation warnings, see #166 (@xuwei-k);
- added better warning for option name guessing failures for unsupported platforms (js and native), see #141;

# v3.1.2
- now -h and -v short option names are generated automatically, if they do not conflict with other user-defined options, see #153;
- added List[File] converters & validators, see #151 (@deugeniy);

# v3.1.1
- decode non-letter characters when guessing option names, see #150;

# v3.1.0
- implemented choice options, see #145;
- added serialization helper for ScallopConf, #137;

# v3.0.3
- really fixed bug with resolving default values even when they are not used, see #143;

# v3.0.2
- fixed bug with resolving default values even when they are not used, see #143;
- names are now always generated for trailing args, see #141;

# v3.0.1
- errors are now output to stderr by default, see #142;

# v3.0.0
- BREAKING: removed old type-unsafe immutable `Scallop` builder, use `ScallopConf` instead;
- cross-build for Scala Native, see #138 (@oker1);
- cross-build for ScalaJS, see #141;
- added help format customization, see #135;

(... to be continued - I don't have the time to go through the rest of commit&release history right now)
