==========
Moderation
==========

.. default-role:: code
.. include:: rstcommon.rst

.. contents::

This document covers main rules that concern moderation of the various
|nimskull| projects and public spaces. This is an initial version, and it
is mostly concerned with establishing rules of interactions when it comes
to accepting and reviewing pull requests, managing github labels,
explaining current project goals and so on.

Helping newcomers
=================

  "The best thing you can do, as a knowledge community member, is to enable
  others to contribute. Small effort on your end could go a long way when
  it comes to helping others - there is no shortage of bright and motivated
  people who are willing contribute, but not everyone has time to figure
  out what needs to be done from scratch"

"PRs are welcome"
=================

It is implied that we welcome all and every contributor, but it is highly
discouraged to tell people that "PRs are welcome" *without any
further elaboration*.

It might
seem dismissive and counterproductive - instead of telling the person asking for a
fix or improvement to "do it yourself" you should elaborate on

- Why we are unable to do this ourselves at the moment (most likely it is caused
  by the lack of manpower and time)
- How this feature can or should be implemented. Maybe someone is willing to
  provide help, but is intimidated by the compiler codebase and uncertain how to even
  approach the problem.
- Why something cannot be implemented or fixed. It might be due to lack of concrete
  details in the original question, clash with other design elements and so on.

All responses should follow the spirit of cooperation instead of dismissing
other's concerns.

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

Prepared replies
----------------

Here is a collection of prepared replies that you can use for point out
common mistakes. They make it easier to include necessary pointers in the
reply (such as link to the necessary parts of the contribution or style
guides), but should not be used as a sole body of the reponse: provided
additional required context.

.. warning::

   Explain what exactly is missing, at least briefly. without elaboration
   it is just an "error. you are wrong" reply that can seem harsh.


.. code::

   Please follow the pr
   [template](https://github.com/nim-works/nimskull/blob/devel/.github/pull_request_template.md?plain=1)
   and commit message
   [guidelines](https://nim-works.github.io/nimskull/contributing.html#commit-message-guidelines-rules).


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
