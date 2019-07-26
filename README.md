# README #

This is a repository for sharing course materials of Advanced Programming at IT University of Copenhagen.
Course website: https://learnit.itu.dk/course/view.php?id=3018481#section-0
If you are a student on the course, you are expected to clone this repository and pull regularly.
If you want to version control your homework solutions, it is probably best to fork the repository, push your solutions to your fork, and sync with upstream regularly. If you decide to fork, then please make your fork private. 

## Contact ##

Any course issues are best to discuss on the course forum on LearnIT (via https://learnit.itu.dk/course/view.php?id=3018481#section-0). The forum is monitored regularly as long as the course lasts.
If you are not a student on the course and have questions about this repo, please contact Andrzej Wasowski (wasowski@itu.dk).

## Software ##

You can run on any operating system you work, but please make sure that you have SBT ver. 0.13.7 or newer installed.

Of course, once in a while, the differences between operating systems appear.  Most often these are caused by other systems you have installed, or configuration changes you made.  If you wanted to limit configuration problems, use the provided Dockerfile. Docker will give you a simple Linux system with sbt installed in which you can work.  A minimal system, minimizes configuration issues, and it allows the teachers to debug your issues in an identical environment on their computers.

Especially Spark and Figaro libraries (in the relevant weeks) might work better on Linux (and so in Docker) than in other systems.

1. Install docker in the preferred way for your OS.

2. In this directory: docker build ./  -t adpro

3. docker run -t adpro sbt 

(you can also run a shell and git to checkout repos, use a text editor, etc)
