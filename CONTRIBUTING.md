# zigSelf Contribution Guidelines

This document outlines how to contribute to zigSelf.

## Where to start

To become familiar with the codebase, new contributors are encouraged to start
with small fixes first. Issues marked with "good first issue" are usually easy
fixes that other people just haven't gotten around to, and new contributors are
recommended to start with them.

## Coding style

Zig code follows standard Zig style. When contributing changes, make sure to run
the changed files through `zig fmt`. Your editor probably has a method to do
this automatically on save.

Self code standards are currently not fully decided upon yet, but a general rule
of thumb is listed below:

- 4 spaces for indentation. No hard tabs.
- All names must be `camelCase`. Keywords other than the first keyword must be
  `PascalCase` (enforced by the parser).
- Primitive names must be `_PascalCase`.
- Code itself can grow to more than 80 characters, but wrap comments at 80
  characters.
- Comments have two different styles:

  - At the top of files, start a comment and follow it up with a newline for
    copyright information:

    ```zigself
    "
    Copyright (c) 20XX, John Doe <john@johndoe.com>

    SPDX-License-Identifier: GPL-3.0-only
    "
    ```

  - Other comments such as documentation for functions should start the comment
    itself immediately after the comment start character, and each subsequent
    line should be indented one character after the comment start character:

    ```zigself
    "This is an example comment which demonstrates how to
     write your comments. Note that lines following the first
     comment line is indented one character further."
    ```

- Objects which are primarily slots objects should open the slot list
  immediately after the opening parentheses and should be listed on multiple
  lines:

  ```zigself
  foo = (|
     "..."
  |).
  ```

- Methods should prefer to keep all their slots on the same line. In case multiple lines are needed (i.e. for inline methods), the slot list closing pipe character should be one indent before the current indentation:

  ```zigself
  foo: arg Bar: anotherArg = (|
      someArg.
      someMethod = (| ... |
          ...
      ).
  |
      ...
  ).
  ```

- Parent slots should be listed first, and all other slots should follow.
- The public interface of an object should be at the top, followed by the
  private implementation details (if any). See
  [`vector.self`](objects/vector.self) for an example.

**TODO:** Add more details.

## Commit style

Each commit title should describe the category of the change, i.e. `runtime:` or
`objects:`. Commit title and description must not exceed 72 characters. The
first word of the commit message after the category must be capitalized and must
be an imperative verb (`fix`, `implement`, `update`, etc.).

An example commit message:

```gitcommit
runtime: Fix some VM problem

The VM had an issue which was then fixed, blah blah blah. The commmit
message was then wrapped at 72 characters.
```

Each commit must be atomic, i.e. all tests should pass and the REPL and other
examples should run. This is to help with bisectability.

## Creating new issues

When a bug is found, please create an issue for it (create a separate issue for
each bug). However, feature requests from non-contributors are usually not
accepted. If you want something to happen, you make it happen. :^)

## Language

All zigSelf code and documentation uses American English in order to be
accessible to as many people as possible. Please use proper spelling, grammar
and punctuation in code, documentation and in commit messages.

## Opening a pull request

When opening a pull request, please ensure that its scope is limited to the set
of commits that you're making. Large PRs with many commits will be hard to
review.

Note however that commits that fix small issues that you come across while
making your intended change is fine, but please split your yak shaves into
relevantly-scoped PRs. :^)

## Applying pull request feedback

After a contributor reviews your pull request and requests changes, please make
any changes requested and update your commitset using git rebase and force-push.
Please do not introduce fixup or merge commits (commits should stay atomic after
you make your changes). If you feel like a requested change is not correct,
respond to the appropriate review comment. After pushing a new set of commits,
mark the requested changes that you have applied as resolved and re-request a
review.
