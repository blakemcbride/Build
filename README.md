
# **BUILD** - a software build system

*build* is a software build system like *make*, *ant*, *maven*, and
*gradle*.  It is being built as a response to a significant
frustration with existing build systems and no known readily available,
open source, solution.  The goals of the *build* system are as follows:

1. Equal ability to build C-like languages as well as Java-like languages.

2. Be easy to understand.

3. Having all of the capabilities of a real programming language.

4. Have all the capabilities of a mature build system such as:

    a. perform only necessary tasks
    
    b. support multiple threads
    
    c. support remote repositories
    
    d. ability to cache intermediaries
    
    e. ability to share intermediaries between several developers
    
    f. support multi-machine, remote builds
    
    g. support projects with possibly 100,000 or more source files
    

## Comparison with other build systems

1.  C and Java, for example, have significantly different build logic.
While any build system can build both C and Java programs when there 
are ten source files, remaining efficient when building a system
with hundreds or thousands of source files is quite a different task.
*make* handles C well.  However, in response to *make's* shortcomings
with respect to the needs of languages such as Java, the industry spawned
*ant*, and then *maven*, and finally *gradle*.  Unfortunately, rather
than solve the problems that *make* has, the other build systems focused
specifically on the needs of Java ignoring the needs and lessons learned 
with *make*.  We thus end up with a class of build systems that are good
and when class of problems and a set of other build system which are 
good at another set of problems.  Neither solve both.  *build* is
designed to solve both.

2.  *make* is explicit and easy to understand.  When building software,
one must know the development language, the problem, and how to solve the
problem with the language.  Rather than assist with the problem solution,
many build systems add a very significant level of complexity to the problem
solution process.  Rather than a solution, many build systems become yet 
another layer of significant complexity.  

    Build systems such as *maven* and *gradle* tout "convention over
    configuration".  What this really means is that if you have a very
    simple system and follow their conventions, the build system works
    nearly automatically.  There is almost nothing to specify.  On the
    other hand, as your needs increase and become increasingly specific
    to your problem domain, "Convention over configuration" build system 
    quickly become inordinately complex.  The complexity of the build process
    starts to become as complex as the application itself thus adding a very
    significant level of complexity and difficulty to support a large and complex
    application.  Unpaid support for complex problems is unavailable since
    the solutions are not widely known.

    With "convention over configuration" systems, the gap between what you
    want and how you describe it is too wide.  There is too much magic between
    what you want and how you describe it to the build system.
    *build* is not a "convention over configuration" system.  Although it
    has rules and procedures to make things easy, the gap between what you
    want and how you describe it is narrow. Further, like *make*, *build*
    is dependency based and not task based since, with a build system,
    that's what you are trying to do *build files* not *run tasks*.

3. Some build systems, such as *ant* and *maven* are bound to XML.
While this may have been cool when the world was XML crazy, XML
is not a language and has many shortcomings that I will not enumerate.
While other build systems, such as *make*, have a few. minor, language
constructs and corresponding flexibilities and power, they lack the
backing and power of a real language thus making the expression of
complex procedures difficult.

    Of the build systems mentioned, only *gradle* has a full, real language
    that can be used to fully express build processes.  Unfortunately,
    *gradle* suffers from poor support of C-like language build procedures,
    and extreme complexity when attempting to go outside their "convention over
    configuration" philosophy.

    *build* provides access to a complete, real language (Common Lisp).

## Why Common Lisp?

Common Lisp was chosen for the following reasons:

1. Common Lisp is the single best language for building domain specific 
languages (DSL).  This means that building *build* would be the easiest
with Common Lisp.

2. Given the unique qualities of Lisp, building a build system wouldn't
necessitate the need for a tokenizer or parser,  This again made the process
of building *build* very significantly easier.

3. These days, Common Lisp runs as fast as C, Java, or C# programs so
there is no speed penalty in choosing Common Lisp.

4. There are many free and open source Common Lisp compilers available
for all platforms.  Thus there are no platform restrictions when choosing
Common Lisp.

5.  Many Common Lisp compilers can produce stand-alone, native programs thus
eliminating the need for any underlying support system needed in order
to run *build*.

6.  Common Lisp is a stable, known ANSI standard.  This means that you are not
using a single-source standard that could change, be dropped, or become
commercial tomorrow.

7.  Common Lisp is perhaps the most powerful language available.  This means
that there are no restrictions on the kinds of things that can be done in
a build system that uses Common Lisp as its language.

## Current Status

*build* is currently in its raw infancy and in the process of being created.
It is unusable.  There are two reasons that it is being put up on *github*
as follows:

1. Use of a common location for the developers to share code.

2. Publicity for this project to attract more developers.

*build* is free and open-source and shall remain so.

Please contact blake1024@gmail.com to get involved in this project.

Thank you.
