==========
Moderation
==========

.. default-role:: code
.. include:: rstcommon.rst

.. contents::

This document covers main rules that concern moderation of the various
nimskull projects and public spaces. This is an initial version, and it is
mostly concerned with establishing rules of interactions when it comes to
accepting and reviewing pull requests, managing github labels, explaining
current project goals and so on.

Helping newcomers
=================

  "The best thing you can do, as a knowledge community member, is to enable
  others to contribute. Small effort on your end could go a long way when
  it comes to helping others - there is no shortage of bright and motivated
  people who are willing contribute, but not everyone has time to figure
  out what needs to be done from scratch"


Managing github labels
======================

In addition to general "bug", "documentation", "enhancement" and similar
labels there are quite a few that were added specifically to distinguish
different actions that need to be taken in order to progress with a pull
request or issue. They must be handed out with care, and each label
assignment needs to be elaborated.

- "Good first issue" - if person assigning such label decided this issue is
  a good fit for a newcomer (that is - person unfamillar with a codebase),
  they should elaborate on how specifically it can be solved. More often
  that not definition of "simple" varies between people.
- "spec needed" - pull request and issue requires a thorough specification
  of the involved parts.

Code reviews
============

When reviewing code it is highly recommended to check whether it complies
with official style guide and point out mismatches.

Examining the changes
---------------------

1. Whenever possible, use GitHub's new 'Suggested change' in code reviews, which
   saves time explaining the change or applying it

2. When reviewing large diffs that may involve code moving around, GitHub's interface
   doesn't help much as it doesn't highlight moves. Instead, you can use something
   like this, see visual results `here <https://github.com/nim-lang/Nim/pull/10431#issuecomment-456968196>`_:

   .. code:: cmd

      git fetch origin pull/10431/head && git checkout FETCH_HEAD
      git diff --color-moved-ws=allow-indentation-change --color-moved=blocks HEAD^

3. In addition, you can view GitHub-like diffs locally to identify what was changed
   within a code block using `diff-highlight`:cmd: or `diff-so-fancy`:cmd:, e.g.:

   ::

      # put this in ~/.gitconfig:
      [core]
        pager = "diff-so-fancy | less -R" # or: use: `diff-highlight`
